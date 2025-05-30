# rm(list = ls())

# Is this going to run on the
#  College of Science's or the College of Engineering's HPC (or neither)?
OnCoSArray <- FALSE
OnCoEArray <- FALSE

OnArray <- OnCoSArray | OnCoEArray

# set to FALSE if you want to watch messages in real time 
# or TRUE to have them silently saved to file instead.
sinkMessages <- !OnArray

# Clear prior fits and errors logs
ClearAll <- !OnArray

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# select the models which are to be fit
holling.like.models <- c(
  "Holling.I",
  "Holling.II",
  "Holling.n",
  "Holling.III",
  "Holling.nIII"
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# How many times to bootstrap each dataset?
# Given no solution to Holling.n model, we need to perform integration 
# for non-replacement studies (which takes a very long time).  
# We therefore bootstrap less often for these.

boot.reps.replacement <- 50
boot.reps.nonreplacement <- 50

fail.ratio <- 3  # Accepted ratio of failed to successful bootstraps

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(ClearAll){
  unlink("../../temp/ErrorLogs/*")
  unlink("../../results/fits/*")
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Utility functions 
source('lib/bootstrap_data.R')
source('lib/plot_coefs.R')
source('lib/resid_metrics.R')
source('lib/set_params.R')
# may throw ignorable warning and takes a while to load because of C++ compiling
source('lib/holling_method_one_predator_one_prey.R')
source('data_subset.R')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load compilation and count unique consumer-resource pairs
load('../../data/datasets.Rdata')

length(datasets)
nrow(
  unique(
  cbind(
  unlist(lapply(datasets, function(x){ x$study.info$pred } )),
  unlist(lapply(datasets, function(x){ x$study.info$prey } )))
  ))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pull out datasets we want to analyze
datasets <- subset_data(datasets, exportSummaries = !OnArray)

# reorder them by sample size
# ss <- order(unlist(lapply(datasets, function(x){
#     x$study.info$sample.size } )), decreasing = TRUE)
# datasets <- datasets[ss]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Let's start analyzing!
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(OnArray){
  if(OnCoSArray){ # Pass both start and end
    ArrayArgs <- commandArgs()
    iStart <- as.integer(ArrayArgs[4])
    iEnd <- as.integer(ArrayArgs[5])
  }
  if(OnCoEArray){
    ArrayArgs <- commandArgs() # Pass only start
    taskID <- as.integer(ArrayArgs[4])
    iStart <- (task_id - 1) * 10 + 1 
    iEnd <- min(iStart + 10, length(datasets)) # ten at a time
  }
} else{
  iStart <- 1
  iEnd <- length(datasets)
}

# selDatasets <-
#   which(unlist(lapply(datasets, function(x){
#     x$study.info$datasetID } )) %in% c(1056))

# Containter for datasets that fail to fit
failed.datasets <- NULL

# # fit everything on a dataset-by-dataset basis
for (i in iStart:iEnd) {
# for (i in selDatasets){

  # grab all of dataset's information
  dataset <- datasets[[i]]
  
  # grab info about experimental design, etc
  this.study <- dataset$study.info
  
  datasetID <- this.study$datasetID
  datasetName <- this.study$datasetName
  
  #############################################
  # fit all the functional response models
  #############################################
  
  # start capturing the warning messages
  errLog <- paste0('../../temp/ErrorLogs/Dataset_', datasetID, '_ErrorLog.txt')
  if (sinkMessages) {
    options(warn = 1) # provide more than just the base info level
    Mesgs <-
      file(errLog, open = 'wt')
    sink(Mesgs, type = "message")
  }
  
  # save data in case we need to bootstrap it
  d.orig <- d <- dataset$data
  
  # Do data need to be bootstrapped?
  # Yes, we now bootstrap for all datasets but distinguish between 
  # parametric (for mean +/- SE studies) and non-parametric (raw data)
  if("Nconsumed.mean" %in% colnames(d)){
    parametric <- TRUE
  } else{
    parametric <- FALSE
  }
  
  # How many times should we bootstrap?
  if(this.study$replacement){
    boot.reps <- boot.reps.replacement
  } else{
    boot.reps <- boot.reps.nonreplacement
  }
  
  skip.hessian <- ifelse(boot.reps == 1, FALSE, TRUE)
  
  # create a progress bar that shows how far along the fitting is
  print(paste0(i, ' of ', length(datasets), ': ', datasetName))
  pb <- txtProgressBar(min = 0,
                       max = boot.reps)
  
  # temporary storage of model fits
  bootstrap.fits <- list()
  
  # perform 1 or many bootstrapped fits
  failed.fit.count <- 0
  for (b in 1:boot.reps) {
    # some bootstrapped data fails for reasons hard to determine
    failed.fit <- TRUE
    while (failed.fit & failed.fit.count < fail.ratio * boot.reps) {
      # generate bootstrapped data if necessary
      if (boot.reps > 1) {
        d <- bootstrap.data(d.orig, 
                            this.study$replacement, 
                            parametric)
      }
      
      # attempt to fit all models and catch an error if any fit fails
      success <- try({
        for (modeltype in c(holling.like.models)) {
          # attempt to fit the model; abort and re-bootstrap if the fit fails
          bootstrap.fits[[modeltype]][[b]] <-
            fit.holling.like(d, 
                             this.study, 
                             modeltype, 
                             skip.hessian = skip.hessian)
        }
      })
      
      # the fit succeeded, so we can continue to the next bootstrap
      if (!inherits(success, "try-error")) {
        failed.fit <- FALSE
        setTxtProgressBar(pb, b)
      }
      
      # the fit failed, so count it
      if (inherits(success, "try-error")) {
        failed.fit.count <- failed.fit.count + 1
      }
    # print(paste('attempt:', b, 'fails:', failed.fit.count))
    }
  }
  close(pb)
  
  if (failed.fit.count == fail.ratio * boot.reps){
        write.table(
          paste(datasetName, b, 'successes'),
          file = paste0('../../temp/ErrorSummaries/FailedToFit-', datasetName, '.txt'),
          row.names = FALSE,
          col.names = FALSE)
    if(!OnArray){
      warning(paste0('Too many failed fits for ', datasetName, ', so skipped'))
    }
  }else{
    for (modeltype in c(holling.like.models)) {
        # create container for the parameter estimates
        # ~~~~~~~~~~~~~~~~~~~~
        # Summarize bootstraps
        # ~~~~~~~~~~~~~~~~~~~~
        # scrape out the parameter estimates
        local.boots <-
          make.array(bootstrap.fits[[modeltype]][[1]], boot.reps)
        
        for (b in 1:boot.reps) {
          local.boots[, , b] <- mytidy(bootstrap.fits[[modeltype]][[b]])
        }
        
        # get out the estimates into their own object
        local.ests <-
          as.array(apply(local.boots, c(1, 2), summarize.boots))
        
        # create container for the logLik of the fits
        local.lls <-
          summarize.boots(sapply(bootstrap.fits[[modeltype]], logLik))
        
        # create container for the AIC of the fits
        local.AICs <-
          summarize.boots(sapply(bootstrap.fits[[modeltype]], AIC))
        
        # create container for the AICc of the fits
        local.AICcs <-
          summarize.boots(sapply(bootstrap.fits[[modeltype]], AICc))
        
        # create container for the BIC of the fits
        local.BICs <-
          summarize.boots(sapply(bootstrap.fits[[modeltype]], BIC))
        
        # create container for the RMSD of the fits
        local.RMSDs <-
          summarize.boots(sapply(bootstrap.fits[[modeltype]], 
                                 resid.metric, metric = 'RMSD'))
        
        # create container for the MAD of the fits
        local.MADs <-
          summarize.boots(sapply(bootstrap.fits[[modeltype]], 
                                 resid.metric, metric = 'MAD'))
        
        # save the key stuff (including the first fit)
        bootstrap.fits[[modeltype]] <- list(
          fit = bootstrap.fits[[modeltype]][[1]],
          boots = local.boots,
          ests = local.ests,
          lls = local.lls,
          AICs = local.AICs,
          AICcs = local.AICcs,
          BICs = local.BICs,
          RMSDs = local.RMSDs,
          MADs = local.MADs
        )
      }
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # save the (first) fit, bootstraps summaries, and some data aspects
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ffr.fit <- list(
        study.info = c(
          datasetName = datasetName,
          sample.size = nrow(d),
          data = d.orig,
          this.study
        ),
        fits = lapply(bootstrap.fits, function(x) x$fit),
        boots = lapply(bootstrap.fits, function(x) x$boots),
        estimates = lapply(bootstrap.fits, function(x) x$ests),
        LL = lapply(bootstrap.fits, function(x) x$lls),
        AIC = lapply(bootstrap.fits, function(x) x$AICs),
        AICc = lapply(bootstrap.fits, function(x) x$AICcs),
        BIC = lapply(bootstrap.fits, function(x) x$BICs),
        RMSD = lapply(bootstrap.fits, function(x) x$RMSDs),
        MAD = lapply(bootstrap.fits, function(x) x$MADs),
        failed.fit.count = failed.fit.count
      )
      
      # Save the data set fit monster object
      saveRDS(ffr.fit,
              file = paste0(
                '../../results/fits/',
                datasetID,
                '.Rdata'
              ))
  }
  # close open streams, etc
  if (sinkMessages) {
    sink(type = "message")
    close(Mesgs)
    options(warn = 0)
    readLines(errLog)
    
    # Remove empty error logs
    docs <-
      list.files('../../temp/ErrorLogs/',
                 pattern = "*.txt",
                 full.names = TRUE)
    file.remove(docs[file.size(docs) == 0])
  }
}

# ~~~~~~~~~~~~~~~~~~~~~
if(OnArray){
  q()
} else{
  closeAllConnections()
}
###########################################################################
###########################################################################
###########################################################################
