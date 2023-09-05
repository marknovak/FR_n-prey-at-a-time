# Given a specified model and an ordered vector of parameter values to be passed to the likelihood function, assign the parameter values to the appropriate parameter names.
# Note: Most parameter values are exponentiated (to avoid negative values during fitting).

set_params<-function(params,
                     model=c('Holling.I',
                             'Holling.II',
                             'Holling.n'
                             )
                     ){
  
  model <- match.arg(model)
  
  if(is.null(params)){
    stop("Must pass 'params' to set_params()")
  }
  
  if(model == "Holling.I"){
    assign('attack',       exp(params[1]), envir = .GlobalEnv)
    assign('handling',     0, envir = .GlobalEnv)
    assign('n',            1, envir = .GlobalEnv)
    
  } else  if(model == "Holling.II"){
      assign('attack',       exp(params[1]), envir = .GlobalEnv)
      assign('handling',     exp(params[2]), envir = .GlobalEnv)
      assign('n',            1, envir = .GlobalEnv)
      
  } else  if(model == "Holling.n"){
    assign('attack',       exp(params[1]), envir = .GlobalEnv)
    assign('handling',     exp(params[2]), envir = .GlobalEnv)
    assign('n',            exp(params[3]), envir = .GlobalEnv)

  }  else  stop("Model not correctly specified in set_params()")
}