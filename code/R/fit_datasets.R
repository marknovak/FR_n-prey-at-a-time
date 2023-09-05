rm(list = ls())
source('lib/data_subset.R')

# set to FALSE if you want to watch messages in real time 
# or are running in parallel, or TRUE to have them silently 
# saved to file instead.
sinkMessages <- TRUE
# Will post error "task 1 failed - cannot open the connection" if
# set to TRUE when running in parallel.

# Clear prior fits and errors logs
ClearAll <- TRUE
if(ClearAll){
  unlink("../../temp/ErrorLogs/*")
  unlink("../../results/fits/*")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load('../../data/datasets.Rdata')
length(datasets)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pull out datasets we want to analyze
datasets <- subset_data(datasets, 
                        printSummaries = TRUE, 
                        dir = '../../temp/ErrorLogs/')
length(datasets)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Let's start analyzing!
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# # fit everything on a dataset-by-dataset basis
# for (i in 1:length(datasets)) {
# for (i in 1:50) {
out <-
  foreach (
    i = 1:50,
    # i = 1:length(datasets),
   .inorder=FALSE) %dopar% {
   # .inorder=FALSE) %do% {
             
    set.seed(i)
  
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 # Placed here to enable parallelization       
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # select the models which are to be fit
   holling.like.models <- c(
     "Holling.I"
     ,
     "Holling.II"
     # ,
     # "Holling.n"
   )


  # Utility functions 
  source('lib/bootstrap_data.R')
  source('lib/mytidySumm.R')
  source('lib/plot_coefs.R')
  source('lib/resid_metrics.R')
  source('lib/set_params.R')
  # may throw ignorable warning and takes a while to load because of C++ compiling
  source('lib/holling_method_one_predator_one_prey.R')
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~        
    
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
  if("Nconsumed.mean" %in% colnames(d)){
    boot.reps <- 2
  } else{
    boot.reps <- 1
  }
  
  # create a progress bar that shows how far along the fitting is
  print(datasetName)
  pb <- txtProgressBar(min = 0,
                       max = boot.reps)
  
  # temporary storage of model fits
  bootstrap.fits <- list()
  
  # perform 1 or many bootstrapped fits
  for (b in 1:boot.reps) {
    # some bootstrapped data fails for reasons hard to determine
    bad.fit <- TRUE
    while (bad.fit) {
      # generate bootstrapped data if necessary
      if (boot.reps > 1) {
        d <- bootstrap.data(d.orig, this.study$replacement)
      }
      
      # attempt to fit all models and catch an error if any fit fails
      success <- try({
        for (modeltype in c(holling.like.models)) {
            # attempt to fit the model; abort and re-bootstrap if the fit fails
              bootstrap.fits[[modeltype]][[b]] <-
                fit.holling.like(d, this.study, modeltype)
        }
      })
      
      # the fit succeeded we can continue to the next bootstrap
      if (!inherits(success, "try-error")) {
        bad.fit <- FALSE
        setTxtProgressBar(pb, b)
      }
    }
  }
  close(pb)
  
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
    MAD = lapply(bootstrap.fits, function(x) x$MADs)
  )
  
  # Save the data set fit monster object
  saveRDS(ffr.fit,
          file = paste0(
            '../../results/fits/',
            datasetID,
            '.Rdata'
          ))
  
  # close open streams, etc
  if (sinkMessages) {
    sink(type = "message")
    close(Mesgs)
    options(warn = 0)
    readLines(errLog)
  }
  
  # Remove empty error logs
  docs <-
    list.files('../../temp/ErrorLogs/',
               pattern = "*.txt",
               full.names = TRUE)
  file.remove(docs[file.size(docs) == 0])
}


###########################################################################
###########################################################################
###########################################################################
