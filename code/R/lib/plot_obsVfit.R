
# Make set_params() available
sp <- list.files(
  "..",
  "set_params.R",
  recursive = TRUE,
  full.names = TRUE,
  include.dirs = TRUE
)
source(sp)

# Function to plot observed vs. predicted and
# calculate pseudo-R2 for each data set and model
plot_obsVfit <- function(ffr.fit,
                         model = c(
                           'Holling.I',
                           'Holling.II',
                           'Holling.n'
                         ),
                         curve = FALSE,
                         title = NULL,
                         ...) {
  model <- match.arg(model)
  
  # Get the data in order
  dataset <- ffr.fit$study.info$datadir
  replacement <- ffr.fit$study.info$replacement
  predators <- ffr.fit$study.info$data.Npredator
  initial <- ffr.fit$study.info$data.Nprey
  
  sample.size <- ffr.fit$study.info$sample.size

  time <- ffr.fit$study.info$data.Time
  # if no times are specified then set to time=1
  if (is.null(time)) {
    time <- 1
  }
  
  if (!is.null(ffr.fit$study.info$data.Nconsumed.se)) {
    eaten <- ffr.fit$study.info$data.Nconsumed.mean
    eaten.se <- ffr.fit$study.info$data.Nconsumed.se
  } else{
    eaten <- ffr.fit$study.info$data.Nconsumed
    eaten.se <- NA
  }
  
  # Get the chosen model's predictions
  params <- coef(ffr.fit$fits[[model]])
  
  set_params(params, model)
  
  # expected number consumed given data and parameters
    Nconsumed.predicted <- holling.like.1pred.1prey(
      N0 = initial,
      a = attack,
      h = handling,
      n = n,
      P = predators,
      T = time,
      modeltype = model,
      replacement = replacement
    )
    
  rmsd <- ffr.fit$RMSD[[model]]['mean']
  
  # extract LL from (last) fit
  LL <- logLik(ffr.fit$fits[[model]])
  
  par(pty = 's')  
  if(!curve){
    rng <- range(c(eaten, Nconsumed.predicted))
    if (!is.null(ffr.fit$study.info$data.Nconsumed.se)) {
      rng <- range(c(rng, eaten - eaten.se, eaten + eaten.se))      
    }
    
    plot(
      eaten,
      Nconsumed.predicted,
      ylim = rng,
      xlim = rng,
      ylab = 'Predicted',
      xlab = 'Observed',
      type = 'n'
    )
    
    if (!is.null(ffr.fit$study.info$data.Nconsumed.se)) {
      suppressWarnings(
        arrows(
          eaten - eaten.se,
          Nconsumed.predicted,
          eaten + eaten.se,
          Nconsumed.predicted,
          angle = 90,
          length = 0.02,
          code = 3
        )
      )
    }
    abline(0, 1, lty = 2)
    
    points(eaten, 
           Nconsumed.predicted, 
           pch = 19)
    
    legend(
      'bottomright',
      legend = c(bquote(RMSD == .(round(rmsd, 2))),
                 bquote(n == .(sample.size))),
      bty = 'n',
      inset = 0,
      cex = 0.8
    )
  }else{
    initial.new <- 1:max(initial)
    
    # expected number consumed given data and parameters
    Nconsumed.predicted <- holling.like.1pred.1prey(
      N0 = initial.new,
      a = attack,
      h = handling,
      n = n,
      P = rep(predators[1], length(initial.new)),
      T = rep(time[1], length(initial.new)),
      modeltype = model,
      replacement = replacement
    )
    
    # # In rare cases the data are so poor / the fit is so bad for the Holling.n
    # # that NaN are produced.  Zap these.
    # Nconsumed.predicted[is.na(Nconsumed.predicted)] <- 0
    
    xrng <- range(0, initial)
    yrng <- range(c(0, eaten, Nconsumed.predicted))
    
    if (!is.null(ffr.fit$study.info$data.Nconsumed.se)) {
      yrng <- range(c(yrng, eaten - eaten.se, eaten + eaten.se))      
    }
    
    plot(initial,
         eaten,
         xlim = xrng,
         ylim = yrng,
         xlab = 'Nprey',
         ylab = 'Eaten',
         type = 'n'
        )
    
    if (!is.null(ffr.fit$study.info$data.Nconsumed.se)) {
      suppressWarnings(
        arrows(
          initial, 
          eaten - eaten.se,
          initial,
          eaten + eaten.se,
          angle = 90,
          length = 0.02,
          code = 3
        )
      )
    }
    
    points(c(initial), 
           c(eaten), 
           pch = 19)
  
    points(c(0, initial.new), 
           c(0, Nconsumed.predicted), 
           type = 'l')
    
    legend(
      'bottomright',
      legend = c(bquote(RMSD == .(round(rmsd, 2))),
                 bquote(n == .(sample.size))),
      bty = 'n',
      inset = 0,
      cex = 0.8
    )
    
  }
  title(title, cex.main = 1, line = 1)
}
