# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A few functions to help with bootstrapping
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# generate boostrapped data
bootstrap.data <-
  function(rawdata, replacement, parametric = TRUE) {
    # When data are given as a set of means and SEs of experimental observations
    if (parametric) {
      # determine the "names" of all prey present
      prey <- grep("[.]mean$", colnames(rawdata), value = TRUE)
      prey <- gsub("[.]mean$", "", prey)
      prey <- gsub("^Nconsumed", "", prey)
      
      # generate a container for the data
      if ("Time" %in% colnames(rawdata)) {
        d <- matrix(NA, 0, 2 + 2 * length(prey))
      } else{
        d <- matrix(NA, 0, 1 + 2 * length(prey))
      }
      
      # go row by row through the data
      for (r in 1:nrow(rawdata)) {
        # if for some reason we do not know the SE or sample size for any of the different observations treat that treatment as having a SINGLE replicate
        if (any(is.na(rawdata[r, paste0("Nconsumed", prey, ".se")])) |
            any(rawdata[r, paste0("Nconsumed", prey, ".se")] == 0) |
            any(is.na(rawdata[r, paste0("n")]))) {
          rawdata[r, paste0("Nconsumed", prey, ".se")] <- 0
          rawdata[r, "n"] <- 1
        }
        
        # go replicate by replicate
        for (e in 1:rawdata[r, "n"]) {
          Na <- c()
          Ne <- c()
          # sample prey by prey
          for (i in prey) {
            # known number of available prey
            Na <- c(Na, rawdata[r, paste0("Nprey", i)])
            
            # sample number of eaten prey ( fr.sample() defined below )
            Ne <- c(
              Ne,
              fr.sample(
                mean = rawdata[r, paste0("Nconsumed", i, ".mean")],
                se = rawdata[r, paste0("Nconsumed", i, ".se")],
                n = rawdata[r, "n"],
                replacement = replacement,
                Nprey = rawdata[r, paste0("Nprey", i)]
              )
            )
          }
          
          # add the requisite info to the new row
          new.row <- c(rawdata[r, "Npredator"],
                       Na,
                       Ne)
          
          # if there was time/duration data in the original data frame, put it back in
          if ("Time" %in% colnames(rawdata)) {
            new.row <- c(new.row,
                         rawdata[r, "Time"])
          }
          
          # add the new "observation" to the data set
          d <- rbind(d, new.row)
        }
      }
      
      # all data frames should emerge with identical column names
      if ("Time" %in% colnames(rawdata)) {
        colnames(d) <- c("Npredator",
                         paste0("Nprey", prey),
                         paste0("Nconsumed", prey),
                         "Time")
      } else{
        colnames(d) <- c("Npredator",
                         paste0("Nprey", prey),
                         paste0("Nconsumed", prey))
      }
    } else{
      # When data represent raw data, perform non-parametric bootstrap
      # (i.e. sample the raw data), distinguishing among stratified
      # datasets (i.e. those with specified treatment levels) and
      # non-stratified datasets (i.e. those where species abundances
      # vary from sample to sample).
      trtmts <- data.frame(table(rawdata[, c(grep("predator", colnames(rawdata)), 
                                             grep("prey$", colnames(rawdata)))]))
   
      # There must be a better way to distinguish!
      stratified <-
        !(nrow(trtmts) > 0.89 * nrow(rawdata)) & all(trtmts$Freq > 1)
  
      if (stratified) {
        sp <- split(seq_len(nrow(rawdata)),
                    list(rawdata$Nprey,
                         rawdata$Npredator))
        samples <- lapply(sp, function(x) {
          sample(x,
                 length(x),
                 replace = TRUE)
        })
        d <- rawdata[unlist(samples),]
      } else{
        d <- rawdata[sample(seq_len(nrow(rawdata)),
                            nrow(rawdata),
                            replace = TRUE),]
      }
    }
    
    d <- data.frame(d, row.names = seq(1, nrow(d)))
    return(d)
  }

###################################################################

# sample a number consumed given mean, se, n, and Nprey
# (which is necessary for non-replacement studies)
fr.sample <- function(mean, se, n, replacement, Nprey = NULL) {
  # we require Nprey for non-replacement since they are proportion of a total which must be specified
  if (is.null(Nprey) && !replacement) {
    stop("fr.sample requires Nprey argument for non-replacement experiment types")
  }
  
  # normal distribution nomenclature
  mu <- mean
  sigma <- se * sqrt(n)
  
  if (!replacement) {
    # calculate the proportion of prey consumed; do not divide by zero prey since no prey available means no prey consumed!
    prob <- ifelse(Nprey == 0,
                   0,
                   rnorm(n = 1, mean = mu, sd = sigma) / Nprey)
    
    # make sure we end up with a valid proportion
    prob <- max(0, min(1, prob))
    
    # bootstrapped number eaten is the result of a binomial process
    Ne <- rbinom(n = 1,
                 size = Nprey,
                 p = prob)
  } else{
    # calculate the expected number consumed given the rate of consumption
    lambda <- rnorm(n = 1, mean = mu, sd = sigma)
    
    # make sure we end up with a valid rate (>=0)
    lambda <- max(0, lambda)
    
    # boostrapped number eaten is the result of a poisson process
    Ne <- rpois(n = 1, lambda = lambda)
  }
  
  return(Ne)
}

###################################################################
mytidy <- function(fit) {
  if (typeof(fit) == 'S4') {
    out <- coef(summary(fit))
    colnames(out) <-
      c('estimate', 'std.error', 'statistic', 'p.value')
    return(out)
  } else{
    return(NA)
  }
}


make.array <- function(ffr.fit, boot.reps) {
  if (typeof(ffr.fit) == 'S4') {
    ffr.fit <- mytidy(ffr.fit)
  }
  out <- array(NA, dim = c(dim(ffr.fit), boot.reps))
  dimnames(out) <- dimnames(ffr.fit)
  return(out)
}

summarize.boots <- function(x) {
  c(
    mean = mean(x, na.rm = TRUE),
    quantile(x, c(0.025, 0.16, 0.5, 0.84, 0.975), na.rm = TRUE),
    n = sum(is.finite(x))
  )
}

###################################################################