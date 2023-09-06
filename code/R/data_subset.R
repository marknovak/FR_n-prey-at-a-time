# Pull out only datasets which we want to analyze

subset_data <- function(datasets, exportSummaries = FALSE, dir = NULL){
  
  if(is.null(dir)){dir <- ''}
  
  datasetIDs <- unlist(lapply(datasets, function(x){ x$study.info$datasetID} ))
  datasetNames <- unlist(lapply(datasets, function(x){ x$study.info$datasetName} )) 
  
  skip.datasets <- c(980) # for some reason doesn't play with Holling.n
  skip.datasets <- datasetIDs == skip.datasets
  
  replacement.study <- 
    unlist(lapply(datasets, function(x){ 
    x$study.info$replacement } ))
  
  more.eaten.than.given <- 
    replacement.study & 
    unlist(lapply(datasets, function(x){
      any(x$data[, grep('Nconsumed', colnames(x$data))[1]] > x$data$Nprey) }))
  
  non.integer.prey <- 
    unlist(lapply(datasets, function(x){
      any(x$data$Nprey != round(as.numeric(x$data$Nprey))) } ))
  
  insufficient.trtmts <- 
    unlist(lapply(datasets, function(x){
      length(unique(x$data$Nprey)) <  3 } ))
  
  keep <- 
    replacement.study &
    !more.eaten.than.given &
    !non.integer.prey &
    !insufficient.trtmts &
    !skip.datasets
    

  if(exportSummaries){
    write.table( cbind( datasetIDs[keep], 
                        datasetNames[keep] ), 
                paste0(dir, 'aaaaNotSkipped.txt'), 
                row.names = FALSE)
    write.table( cbind( datasetIDs[!keep], 
                        datasetNames[!keep] ),  
                paste0(dir, 'aaaaSkipped.txt'), 
                row.names = FALSE)
    write.table( cbind( datasetIDs[more.eaten.than.given], 
                        datasetNames[more.eaten.than.given] ), 
                paste0(dir, 'aaaTooManyEaten.txt'),
                row.names = FALSE)
    write.table( cbind( datasetIDs[non.integer.prey], 
                        datasetNames[non.integer.prey] ),
                paste0(dir, 'aaaNonIntegerPrey.txt'),
                row.names = FALSE)
    write.table( cbind( datasetIDs[insufficient.trtmts], 
                        datasetNames[insufficient.trtmts] ),
                paste0(dir, 'aaaInsufficientTreatments.txt'),
                row.names = FALSE)
  }
  
  message(paste0('Count of skipped datasets: ', sum(!keep)))
  message(paste0('Count of not-skipped datasets: ', sum(keep)))

  return(datasets[keep])

}



