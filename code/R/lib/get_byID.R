get_byID <- function(ffr.fits, ID = null){
  IDs <- unlist(lapply(ffr.fits, function(x)x$study.info$datasetID))
  if(sum(ID==IDs)==1){
    return(ffr.fits[[which(ID==IDs)]])
  }else{
    return(NA)
  }
}