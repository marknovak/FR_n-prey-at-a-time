########################################################
# Clean-up, organize, and save FoRAGE datasets to a list
########################################################
options(warn = 1)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
forage_meta <- read.csv('../../data/FoRAGE_db_V3_Jan_2_2023.csv')
forage <-
  read.csv('../../data/FoRAGE_db_V3_Jan_2_2023_original_curves.csv')
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

forage$Rate.is <- tolower(forage$Rate.is)
forage$Foraging.rate.units <- trimws(forage$Foraging.rate.units)
suppressWarnings(
  forage$Sample.size..per.density. <-
  as.numeric(forage$Sample.size..per.density.)
)

forage$Original.y[forage$Original.y < 0] <- 0
forage$Foraging.rate[forage$Foraging.rate < 0] <- 0

suppressWarnings(
  forage$Trial.duration..h. <- as.numeric(forage$Trial.duration..h.)
)
forage$Trial.duration..h.[forage$Trial.duration..h. == ''] <- 1
forage$Trial.duration..h.[is.na(forage$Trial.duration..h.)] <- 1

# if no error or sample size are given for mean studies
sel <- forage$Rate.is == 'mean' & is.na(forage$Original.error) | 
  forage$Rate.is == 'mean' & forage$Original.error == ''
forage$Original.error[sel] <- 0
sel <- forage$Rate.is == 'mean' & is.na(forage$Sample.size..per.density.) | 
  forage$Rate.is == 'mean' & forage$Sample.size..per.density. == ''
forage$Sample.size..per.density.[sel] <- 1

# Standardize original error to SE units
forage$Original.error.units[grep('95',forage$Original.error.units)] <- '95%CI'
forage$Original.error.units[grep('SE',forage$Original.error.units)] <- 'SE'
forage$Original.Standard.error <- NA
sel <- forage$Rate.is=='mean' & forage$Original.error.units=='SE' | 
  forage$Rate.is=='mean' & forage$Original.error.units == ''
forage$Original.Standard.error[sel] <- forage$Original.error[sel]
sel <- forage$Rate.is=='mean' & forage$Original.error.units=='95%CI'
forage$Original.Standard.error[sel] <- forage$Original.error[sel] / 1.96
sel <- forage$Rate.is=='mean' & forage$Original.error.units=='SD'
forage$Original.Standard.error[sel] <- forage$Original.error[sel] / 
                                          sqrt(forage$Sample.size..per.density.[sel])
sel <- forage$Rate.is=='mean' & forage$Original.error.units=='Range'
forage$Original.Standard.error[sel] <- forage$Original.error[sel] / 2 # As assumed by DeLong & Uiterwaal

forage_meta$Prey.replaced.[forage_meta$Prey.replaced. == 'N'] <-  FALSE
forage_meta$Prey.replaced.[forage_meta$Prey.replaced. == 'Y'] <-  TRUE
forage_meta$Prey.replaced. <- as.logical(forage_meta$Prey.replaced.)
forage_meta$Field.[forage_meta$Field. == ''] <- FALSE
forage_meta$Field.[forage_meta$Field. == 'Y'] <- TRUE
forage_meta$Field. <- as.logical(forage_meta$Field.)
forage_meta$Interference <- trimws(forage_meta$Interference)
forage_meta$Interference[forage_meta$Interference == 'Y'] <- TRUE
forage_meta$Interference[forage_meta$Interference == ''] <- FALSE
forage_meta$Interference <- as.logical(forage_meta$Interference)

# The next throws errors for studies that don't have the correct predator abundances
# reported.  For now assume they had a single predator.
suppressWarnings(
  forage_meta$Pred.per.arena <- as.numeric(forage_meta$Pred.per.arena)
)
suspect <- is.na(forage_meta$Pred.per.arena) | 
  forage_meta$Pred.per.arena == ''
# write.table(forage_meta[suspect, c('Data.set','Source')],'suspect.txt')
forage_meta$Pred.per.arena[suspect] <- 1

# Drop treatments that have zero prey offered
forage$Original.x <- as.numeric(forage$Original.x)
keep <- forage$Original.x != 0
forage <- forage[keep,]

# Drop datasets that aren't present in both sheets
keep <- unique(forage$Data.set) %in% unique(forage_meta$Data.set)
forage <- forage[keep,]
keep <- unique(forage_meta$Data.set) %in% unique(forage$Data.set)
forage_meta <- forage_meta[keep,]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

datasetIDs <- unique(forage$Data.set)
datasets <- vector(mode = 'list', 
                   length = length(ndatasets))

pb <- txtProgressBar(min = 0,
                     max = length(datasetIDs))
b <- 1

for (i in 1:length(datasetIDs)) {
  temp.study <- subset(forage, forage$Data.set == datasetIDs[i])
  temp.info <- subset(forage_meta, forage_meta$Data.set == datasetIDs[i])

  study.info <- list(
    datasetID = datasetIDs[i],
    datasetName = paste(datasetIDs[i], unique(temp.study$Source), sep='-'),
    source = unique(temp.study$Source),
    pred = unique(temp.study$Predator),
    prey = unique(temp.study$Prey),
    type = unique(temp.study$Rate.is),
    replacement = unique(temp.info$Prey.replaced.),
    dimension = unique(temp.info$Dim),
    field = unique(temp.info$Field.),
    habitat = unique(temp.info$Habitat),
    fresh.marine = unique(temp.info$Fresh.or.Marine),
    predator.parasite = unique(temp.info$Predation.or.Parasitism),
    predator.cellularity = unique(temp.info$Predator.cellularity),
    prey.cellularity = unique(temp.info$Prey.cellularity),
    predator.mass = unique(temp.info$Predator.mass..mg.),
    prey.mass = unique(temp.info$Prey.mass..mg.),
    pred.vert.invert = unique(temp.info$Vert.invert),
    prey.vert.invert = unique(temp.info$Vert.invert.1),
    pred.major.grouping = unique(temp.info$Major.grouping.1),
    prey.major.grouping = unique(temp.info$Major.grouping.2),
    prey.type = unique(temp.info$Prey.type),
    interference.study = unique(temp.info$Interference),
    temperature = unique(temp.info$Temp..C..),
    pred.per.arena = unique(temp.info$Pred.per.arena)
  )
  
  if (any(lapply(study.info, length) > 1)) {
    stop(paste('Dataset ', datasetIDs[i], ' has at least one non-unique attribute.'))
    print(study.info)
  }
  
  data.orig <- temp.study[,
                          c(
                            'Original.x',
                            'Original.x.units',
                            'Original.y',
                            'Original.y.units',
                            'Original.error',
                            'Original.error.units',
                            'Trial.duration..h.',
                            'Sample.size..per.density.',
                            'X2D.Arena.size',
                            'X2D.Arena..units',
                            'X3D.Arena.size',
                            'X3D.Arena..units',
                            'X2D.Prey.density',
                            'X2D.density.units',
                            'X3D.Prey.density',
                            'X3D.density.units',
                            'Foraging.rate',
                            'Foraging.rate.units',
                            'Standard.Error',
                            'Original.Standard.error'
                          )]
  
  ## Can't use area/volume-standardized values since non-integer values are
  ## not consistent with binomial and poisson models
  # if (all(!is.na(data.orig$X2D.Prey.density))) {
  #   x <- data.orig$X2D.Prey.density
  # } else{
  #   x <- data.orig$X3D.Prey.density
  # }
  # y <- data.orig$Foraging.rate
  # se <- data.orig$Standard.Error
  # t <- 1 # 'Foraging.rate' is already standardized to 'per day'
  
  x <- data.orig$Original.x
  y <- data.orig$Original.y
  se <- data.orig$Original.Standard.error
  t <- data.orig$Trial.duration..h.
  
  n <- data.orig$Sample.size..per.density.
  p <- rep(as.numeric(study.info$pred.per.arena), length(x))

  data <- data.frame(
    Nprey = x,
    Npredator = p,
    Nconsumed = y,
    Nconsumed.se = se,
    n = n,
    Time = t
  )
  
  
   # if any "raw data" consumption is reported as non-integer, treat as mean dataset
  if(study.info$type == 'raw data' & any(data$Nconsumed != round(data$Nconsumed, 0))) {
    study.info$type <- 'mean'
    data$Nconsumed.se <- 0
    data$n <- 1
    }
  
  if(study.info$type == 'mean') {
    colnames(data)[3] <- 'Nconsumed.mean'
  } else{
    data <- data[, -c(4, 5)] # remove 'se' and 'n' from raw data studies
  }
  

  
  # Density ratios (to standardize attack rates post-hoc)
  study.info$Prey.density.ratio <- 
    ifelse(study.info$dimension == 3,
           mean(data.orig$X3D.Prey.density / data.orig$Original.x),
           mean(data.orig$X2D.Prey.density / data.orig$Original.x))
         

  datasets[[i]] <- list(data = data,
                        study.info = study.info,
                        orig = data.orig)
  b <- b + 1
  setTxtProgressBar(pb, b)
}
close(pb)

save(datasets,
     file = '../../data/datasets.Rdata')

########################################################
########################################################
########################################################
