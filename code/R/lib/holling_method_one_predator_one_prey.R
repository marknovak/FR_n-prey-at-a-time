#############################################
# holling-like functional responses
#############################################
# For non-replacement datasets, we have two options : 
# (1) solve using lambertsW (or solve transcendental eqn directly if needed)
# (2) integrate
#############################################
# libraries required
library(bbmle)
library(nloptr)
library(lamW)
library(odeintr)

sp <- list.files("..", "set_params.R", 
                 recursive=TRUE, 
                 full.names=TRUE, 
                 include.dirs=TRUE)
source(sp)
#############################################

# For integration method, define the ode in C++ format
# Note that exponent terms are not n and (n+1) for numerator and denominator
# respectively, but rather are (n+1) and (n+2) for numerator and denominator
# respectively in order to constrain "actual" n (of original model) to be > 1.
holling.like.1pred.1prey.sys = '
  #include<cmath>
	// functional response for one predator one prey
	dxdt[0] = -P * (a * x[0] * (1 - pow( a * h * x[0], (n + 1) ))) / (1 - pow( a * h * x[0], (n + 1 + 1)));
	// consumption rate cannot be positive
	if(dxdt[0] > 0) dxdt[0] = 0;
'

# compile the above C++ code into something we can run in R
odeintr::compile_sys(
	"hl_1pred_1prey",
	holling.like.1pred.1prey.sys,
	pars = c("a", "h", "n", "P"),
	method = "rk78"
)

# predicted number of species consumed given parameters of a holling-like functional response
holling.like.1pred.1prey = function(N0, a, h, n, P, T,
                                    replacement,
                                    modeltype,
                                    integrate=FALSE,
                                    overrideTranscendental=FALSE){

	# in a world with replacement everything is hunky dory
  # (again note +1 in exponent terms; see above)
	if(replacement){
		numer <- a * N0 * (1 - (a * h * N0)^(n + 1) )
		denom <- (1 - (a * h * N0)^(n + 1 + 1))
		N <- (numer / denom) * P * T
		N <- pmax(0,N)
		return(N)
	}

	# without replacement
	if(!replacement){
		if(h==0){ # For Type I things are simple:
			N <- N0 * (1 - exp(-a * P * T))
		} else { # For all other models...
			if(integrate | modeltype == 'Holling.n'){  # solve by direct integration
				N <- numeric(length(N0))
				for(i in seq.int(length(N0))){
					# set parameters within ode solver
					hl_1pred_1prey_set_params(
						a=a,
						h=h,
						n=n,
						P=P[i]
					)
				  
					# calculate the final number of prey integrating the ode
					Nfinal <- hl_1pred_1prey(N0[i], T[i], T[i]/1000.)

					# we only need the last row since this is the final "abundance"
					Nfinal <- as.numeric(Nfinal[nrow(Nfinal), 2])
					
					# on rare occasions we dip into negative final abundances 
					 # (that tend to be extremely small)
					Nfinal <- pmax(0, Nfinal)
					
					# let's also make sure no prey magically appear
					Nfinal <- pmin(Nfinal, N0[i])
					
					# when N0 is too low it's possible we get NaN returned
					Nfinal <- ifelse(is.nan(Nfinal), N0[i], Nfinal)

					# the number consumed is the difference between what we started with and what is left
					N[i] <- N0[i] - Nfinal
				}
			} else {	# solve using lambertsW (or trancendental equation)
      			N <- N0 - (1 / (a * h)) * 
      			  lamW::lambertW0(((a * h * N0)) * 
      			                    exp(- a * (P * T - h * N0)))

      			# sometimes the argument in the exponential passed to lambertW0 causes it to blow up
      			if(!overrideTranscendental){
					if(any(is.infinite(N))){
						# the explicit result of the analytical integration without solving for N implictly
						ffff <- function(N, N0, P, T, a, h, n){
							dN <- log((N0 - N) / N0) - a * h * N
							dt <- - a * P * T
							dN - dt
						}
						# sometimes the time argument is a constant and not a vector
						if(length(T)==1){
							T <- rep(T, length(N0))
						}
						# check which predictions are non-sensical
						for(i in 1:length(N0)){
							if(is.infinite(N[i])){
								# we need to solve the transcendental equation directly
								nn <- uniroot(ffff, lower=0, upper=N0[i], 
								              N0=N0[i], P=P[i], T=T[i], 
								              a=a, h=h, n=n)
								N[i] <- nn$root
							}
						}
					}
				}
			}
		}
		return(N)
	}

	stop()
}

holling.like.1pred.1prey.predict = function(
	params,
	modeltype,
	initial,
	predators,
	replacement,
	time=NULL
){
	# perform parameter transformations etc
	set_params(params, modeltype)

	# if no times are specified then normalize to time=1
	if(is.null(time)){
		time <- rep(1,length(initial))
	}

	Nconsumed <- holling.like.1pred.1prey(
		N0=initial,
		a=attack,
		h=handling,
    n=n,
		P=predators,
		T=time,
		replacement=replacement,
		modeltype=modeltype
	)

	return(Nconsumed)
}

# negative log likelihood for holling-like models given parameters and requisite data
holling.like.1pred.1prey.NLL = function(params,
                                        modeltype,
                                        initial,
                                        killed,
                                        predators,
                                        replacement,
                                        time=NULL,
                                        penalized.nll = FALSE,
                                        lambda = 1/log(40)
                                        ){

	# expected number consumed given data and parameters
	Nconsumed <- holling.like.1pred.1prey.predict(
		params,
		modeltype=modeltype,
		initial=initial,
		predators=predators,
		replacement=replacement,
		time=time
	)

	# if the parameters are not biologically plausible, neither should be the likelihood
	if(any(Nconsumed <= 0) | any(!is.finite(Nconsumed))){
		nll <- Inf
		return(nll)
	}else{
		# negative log likelihood based on proportion consumed (no replacement)
		if(!replacement){
			# warnings suppressed because direct integration can return prob = 0 or 1, 
		  # which results in NaNs
			nll <- suppressWarnings( 
			  -sum(dbinom(killed, prob=Nconsumed/initial, size=initial, log=TRUE)) 
			  )
			if(is.nan(nll)){
			  nll <- Inf
			}
			# return(nll)
		}

		# negative log likelihood based on total number consumed (replacement)
		if(replacement){
			nll <- -sum(dpois(killed, Nconsumed, log=TRUE))
			# return(nll)
		}
	  
	  if(penalized.nll){
	    # Penalize the nll by the value of 'n' (recalling that, given the structure
	    # of the parameterized n-prey model, we must add 1 to n to get "true" n)
	    # A lambda of 1/3 roughly corresponds to a value of n = 20 counting as 1/2
	    # a unit of log-likelihood
	    set_params(params, modeltype)
	    nll <- nll + lambda * log(1 + n)
	  }
	  
	  return(nll)
	}

	stop()
}

# needed by mle2 to pass named parameters in the right order
# DEBUG there must be a more elegant way to do this "within" the function itself
parnames(holling.like.1pred.1prey.NLL) <- c(
	'attack',
	'handling',
	'n'
)

# given data (d), study info (s), and modeltype (e.g., "Holling I"), fit functional response data
fit.holling.like <- function(
	d,
	s,
	modeltype,
	nloptr.control=list(),
	mle2.control=list(),
	skip.hessian = FALSE,
	...
){

	# estimate starting value from the data using linear regression
	start <- list(
		attack = log(coef(lm(d$Nconsumed~ 0+ I(d$Npredator * d$Nprey * d$Time))))
	)

	# fit Holling Type I via MLE with above starting parameter value
	hollingI.via.sbplx <- nloptr::sbplx(
		x0 = unlist(start),
		fn = holling.like.1pred.1prey.NLL,
		modeltype="Holling.I",
		initial=d$Nprey,
		killed=d$Nconsumed,
		predators=d$Npredator,
		time=d$Time,
		replacement=s$replacement,
		control = nloptr.control,
		...
	)

	# refit with mle2 since this also estimates the covariance matrix for the parameters
	mle2.start <- as.list(hollingI.via.sbplx$par)
	names(mle2.start) <- names(start)

	hollingI.via.mle2 <- bbmle::mle2(
		minuslogl = holling.like.1pred.1prey.NLL,
		start = mle2.start,
		data = list(
			initial=d$Nprey,
			killed=d$Nconsumed,
			predators=d$Npredator,
			time=d$Time,
			replacement=s$replacement,
			modeltype="Holling.I"
		),
		vecpar = TRUE,
		control = mle2.control,
		skip.hessian = skip.hessian,
		...
	)

	if(modeltype == "Holling.I"){
		return(hollingI.via.mle2)
	}
	# code to fit subsequent models
	else{
		# fit Holling II first
		start <- list(
			attack = coef(hollingI.via.mle2)["attack"],
			handling = log(1)
		)

		# fit the more complex model with sbplx first
		hollingII.via.sbplx <- nloptr::sbplx(
			x0 = unlist(start),
			fn = holling.like.1pred.1prey.NLL,
			modeltype = "Holling.II",
			initial = d$Nprey,
			killed = d$Nconsumed,
			predators = d$Npredator,
			time = d$Time,
			replacement = s$replacement,
			control = nloptr.control
		)

		mle2.start <- as.list(hollingII.via.sbplx$par)
		names(mle2.start) <- names(start)

		# refit with mle2 since this also estimates the covariance matrix for the parameters
		hollingII.via.mle2 <- bbmle::mle2(
			minuslogl = holling.like.1pred.1prey.NLL,
			start = mle2.start,
			data = list(
				initial = d$Nprey,
				killed = d$Nconsumed,
				predators = d$Npredator,
				time = d$Time,
				replacement = s$replacement,
				modeltype = "Holling.II"
			),
			vecpar = TRUE,
			control = mle2.control,
			skip.hessian = skip.hessian
		)

		if(modeltype == "Holling.II"){
			return(hollingII.via.mle2)
		}else{
			if(modeltype == "Holling.n"){
				start <- list(
					attack = coef(hollingII.via.mle2)["attack"],
					handling = coef(hollingII.via.mle2)["handling"],
					n = log(1) # given structural parameterization as 1+n
				)
			}

			# fit the more complex model with sbplx first
			fit.via.sbplx <- nloptr::sbplx(
				x0 = unlist(start),
				fn = holling.like.1pred.1prey.NLL,
				modeltype = modeltype,
				initial = d$Nprey,
				killed = d$Nconsumed,
				predators = d$Npredator,
				time = d$Time,
				replacement = s$replacement,
				control = nloptr.control,
				...
			)

			# convert nloptr estimation to list of starting values
			mle2.start <- as.list(fit.via.sbplx$par)
			names(mle2.start) <- names(start)

			# fit with mle2 since this provides other convenience estimates
			fit.via.mle2 <- bbmle::mle2(
				minuslogl = holling.like.1pred.1prey.NLL,
				start = mle2.start,
				data = list(
					initial = d$Nprey,
					killed = d$Nconsumed,
					predators = d$Npredator,
					time = d$Time,
					replacement = s$replacement,
					modeltype = modeltype
				),
				vecpar = TRUE,
				control = mle2.control,
				skip.hessian = skip.hessian
			)

			# convert mle2 estimation to list of starting values
			mle2.start <- as.list(fit.via.mle2@coef)
			names(mle2.start) <- names(start)

			# apparently this helps optimize over complex likelihood surfaces and get SEs when they weren't there otherwise...
			mle2.control$parscale <- abs(fit.via.mle2@coef)

			# refit with mle2 using parscale to help get an appropriate covariance matrix for the parameters
			refit.via.mle2 <- bbmle::mle2(
				minuslogl = holling.like.1pred.1prey.NLL,
				start = mle2.start,
				data = list(
					initial = d$Nprey,
					killed = d$Nconsumed,
					predators = d$Npredator,
					time = d$Time,
					replacement = s$replacement,
					modeltype = modeltype
				),
				vecpar = TRUE,
				control = mle2.control,
				skip.hessian = skip.hessian
			)

			# add the first set of starting values as these can help when profiling
			refit.via.mle2@call$data$sbplx.start <- unlist(start)
			names(refit.via.mle2@call$data$sbplx.start) <- names(start)

			return(refit.via.mle2)

		}
	}
}
