source('data_subset.R')
load('../../data/datasets.Rdata')
datasets <- subset_data(datasets, exportSummaries = FALSE)


labels <-   
  unlist(lapply(datasets, function(x){ 
  x$study.info$datasetName } ))
  
replacement.study <- 
  unlist(lapply(datasets, function(x){ 
    x$study.info$replacement } ))


reps <- 
  unlist(lapply(datasets, function(x){
    x$study.info$sample.size } ))

rescaled.eaten <-
  unlist(lapply(datasets, function(x){
    x$study.info$rescaled.eaten.scaling.factor } ))
table(rescaled.eaten)

rescaled.prey <-
  unlist(lapply(datasets, function(x){
    x$study.info$rescaled.prey.scaling.factor } ))
table(rescaled.prey)

labels[which(rescaled.eaten > 10)]

labels[which(rescaled.prey > 30)]
