is.wholenumber <- function(x, tol = .Machine$double.eps^0.2){
  if(is.numeric(x)){abs(x - round(x)) < tol}else{NA}
  }

# Pull out only datasets which we want to analyze
subset_data <- function(datasets, exportSummaries = FALSE, 
                        dir = '../../temp/ErrorSummaries/'){
  
  if(is.null(dir)){dir <- ''}
  
  datasetIDs <- unlist(lapply(datasets, function(x){ x$study.info$datasetID} ))
  datasetNames <- unlist(lapply(datasets, function(x){ x$study.info$datasetName} )) 
  
  # insert datasetIDs to skip specific datasets
  skip.datasets <- c(672, # Tully et al 2005 varied (unknown) arena sizes
                     1693, # Vucetich et al 2002 predator numbers vary in unknown way
                     2379:2385,  # Reeve 1963 variable (unknown) volumes
                     785:791 ) # Båmstedt 1990 variable durations and predator numbers
  
  skip.datasets <- datasetIDs %in% skip.datasets
  
  replacement.study <- 
    unlist(lapply(datasets, function(x){ 
    x$study.info$replacement } ))
  
  non.numeric.predators <- 
    unlist(lapply(datasets, function(x){
      any(is.na(x$data$Npredator) | x$data$Npredator == '') } ))
  
  more.eaten.than.given <-
    unlist(lapply(datasets, function(x){
      x$study.info$rescaled.prey } ))
  
  more.eaten.than.given.num <-
    unlist(lapply(datasets, function(x){
      x$study.info$rescaled.prey.scaling.factor } ))
  
  insufficient.trtmts <- 
    unlist(lapply(datasets, function(x){
      length(unique(x$data$Nprey)) <  4 } ))
  
  insufficient.trtmts.num <- 
    unlist(lapply(datasets, function(x){
      length(unique(x$data$Nprey)) } ))
  
  insufficient.reps <- 
    unlist(lapply(datasets, function(x){
      x$study.info$sample.size <  15 } ))
  
  insufficient.reps.num <- 
    unlist(lapply(datasets, function(x){
      x$study.info$sample.size } ))
  
  mass.study <-
    unlist(lapply(datasets, function(x){
      x$study.info$mass.study } ))
  
  non.integer.prey <- 
    !mass.study &
    unlist(lapply(datasets, function(x){
      any(!is.wholenumber(x$data$Nprey)) } ))
  
  non.mean.study <-
    unlist(lapply(datasets, function(x){
      x$study.info$type != 'mean' } ))
  
  non.integer.eaten <- 
    non.mean.study &
    !mass.study &
    !non.numeric.predators &
    unlist(lapply(datasets, function(x){
      any(!is.wholenumber(x$data$Nconsumed)) } ))
  
  
  # ----->  EDIT HERE AS DESIRED  <------ #
  keep <- 
    # replacement.study &
    !non.integer.prey &
    !non.integer.eaten &
    !non.numeric.predators &
    !insufficient.trtmts &
    !insufficient.reps &
    # !mass.study &
    !skip.datasets
  # ----------------------------------   #  

  if(exportSummaries){
    write.table( datasetNames[keep], 
                paste0(dir, 'aNotSkipped.txt'), 
                row.names = FALSE)
    write.table( datasetNames[!keep],  
                paste0(dir, 'aSkipped.txt'), 
                row.names = FALSE)
    write.table( cbind(datasetNames[more.eaten.than.given], 
                       more.eaten.than.given.num[more.eaten.than.given.num > 0 & 
                                                   !is.na(more.eaten.than.given.num)]), 
                paste0(dir, 'TooManyEaten.txt'),
                row.names = FALSE)
    write.table( datasetNames[non.integer.prey],
                paste0(dir, 'NonIntegerPrey.txt'),
                row.names = FALSE)
    write.table( datasetNames[non.integer.eaten],
                 paste0(dir, 'NonIntegerEaten.txt'),
                 row.names = FALSE)
    write.table( datasetNames[non.numeric.predators],
                 paste0(dir, 'NonNumericPredators.txt'),
                 row.names = FALSE)
    write.table( cbind(datasetNames[insufficient.reps], 
                       insufficient.reps.num[insufficient.reps]), 
                 paste0(dir, 'InsufficientReplicates.txt'),
                 row.names = FALSE)
    write.table( cbind(datasetNames[insufficient.trtmts], 
                       insufficient.trtmts.num[insufficient.trtmts]), 
                 paste0(dir, 'InsufficientTreatments.txt'),
                 row.names = FALSE)
    write.table( datasetNames[mass.study],
                 paste0(dir, 'MassStudy.txt'),
                 row.names = FALSE)
  }
  
  message(paste0('Total number of potential datasets: ', length(datasets)))
  message(paste0('Count of skipped datasets: ', sum(!keep)))
  message(paste0('Count of not-skipped datasets: ', sum(keep)))

  return(datasets[keep])

}



