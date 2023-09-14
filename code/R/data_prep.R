########################################################
# Clean-up, organize, and save FoRAGE datasets to a list
########################################################
options(warn = 1)
rm(list = ls())

# Convenience function
is.wholenumber <- function(x, tol = .Machine$double.eps^0.2){abs(x - round(x)) < tol}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
forage_meta <- read.csv('../../data/FoRAGE_db_V3_Jan_2_2023.csv')
forage <-
  read.csv('../../data/FoRAGE_db_V3_Jan_2_2023_original_curves.csv')
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~
# Basic cleanup ----
# ~~~~~~~~~~~~~
colnames(forage)[1] <- 'Data.set'

# Remove extra columns
sel <- !grepl('X.\\d{1,2}$', colnames(forage))
forage <- forage[, sel]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dataset-specific typo fixes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
sel <- forage$Data.set == 501 | forage$Data.set == 502
forage$Source[sel] <- 'Walker and Rypstra 2001'

sel <- forage$Data.set == 2584
forage$X2D.density.units[sel] <- 'cm2'

sel <- forage$Data.set == 98 & forage$Original.x == 10 &  forage$Original.y > 70
forage$Original.y[sel] <- 7

sel <- forage$Data.set == 296 & forage$Original.y == 15.75
forage$Original.y[sel] <- 0.75

sel <- forage$Data.set == 335 & forage$Original.x == 19
forage <- forage[!sel, ]

sel <- forage$Data.set %in% 2235:2238
forage$Trial.duration..h.[sel] <- 24

sel <- forage$Data.set == 296 & forage$Original.x == 4 &  forage$Original.y > 4
forage$Original.y[sel] <- forage$Original.y[sel] / 10

sel <- forage$Data.set == 917 & forage$Original.x == 80 &  forage$Original.y > 4
forage$Original.y[sel] <- forage$Original.y[sel] / 10

sel <- forage$Data.set == 2091 & forage$Original.x == 128 &  forage$Original.y == 399
forage$Original.y[sel] <- 39

sel <- forage$Data.set == 1439
forage$Original.y[sel] <- round(forage$Original.y[sel], 3)

sel <- forage$Data.set == 1794
forage$Original.y[sel] <- round(forage$Original.y[sel], 2)

sel <- forage$Data.set %in% 2680:2681
forage$Original.y[sel] <- round(forage$Original.y[sel], 2)

sel <- forage$Data.set %in% 2682:2699
forage$Original.y[sel] <- round(forage$Original.y[sel], 2)

sel <- forage$Data.set %in% 2700:2701
forage$Original.y[sel] <- round(forage$Original.y[sel], 2)

sel <- forage$Data.set %in% 2704:2706
forage$Original.y[sel] <- round(forage$Original.y[sel], 2)

sel <- forage$Data.set == 1633 & is.na(forage$Original.error)
forage$Original.error[sel] <- 20.2 # assume minimum of given values

# ~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~
# Shorter study name
# ~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~
forage$AuthorYear <-   sub("^([^0-9]*\\d+).*", "\\1", forage$Source)
forage_meta$AuthorYear <-   sub("^([^0-9]*\\d+).*", "\\1", forage_meta$Source)


# ~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~
# Unit descriptions ----
# ~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~
# Data type descriptor
forage$Rate.is <- tolower(forage$Rate.is)


# DeLong-calculated foraging rate units (though we won't use them)
forage$Foraging.rate.units <- trimws(forage$Foraging.rate.units)


# Units of arena area and volume
forage$X2D.Arena..units <- trimws(forage$X2D.Arena..units)
forage$X3D.Arena..units <- trimws(forage$X3D.Arena..units)


# Inspect arena unit combinations
unique(forage[, c('X2D.Arena..units', 'X3D.Arena..units')])


# Create new x and y on which to affect changes
forage$PreyAvail <- forage$Original.x
forage$PreyEaten <- forage$Original.y


# Standardize units of prey available
sort(unique(forage[, c('Original.x.units')]))
forage$PreyAvail.units <- tolower(forage$Original.x.units)

forage$PreyAvail.units <- gsub('per per', 'prey per', 
                               forage$PreyAvail.units)
forage$PreyAvail.units <- gsub('of prey', 'prey', 
                               forage$PreyAvail.units)
forage$PreyAvail.units <- gsub('number', 'prey', 
                               forage$PreyAvail.units)
forage$PreyAvail.units <- gsub('prey per available', 'prey', 
                               forage$PreyAvail.units)
forage$PreyAvail.units <- gsub('offered', '', 
                               forage$PreyAvail.units)
forage$PreyAvail.units <- gsub('um3 per', 'um3 prey per', 
                               forage$PreyAvail.units)
forage$PreyAvail.units <- gsub('mg c per', 'mg c prey per', 
                               forage$PreyAvail.units)
forage$PreyAvail.units <- gsub('prey pigment', 'prey', 
                               forage$PreyAvail.units)
forage$PreyAvail.units <- gsub('ug', 'µg', 
                               forage$PreyAvail.units)
forage$PreyAvail.units <- gsub('um', 'µm', 
                               forage$PreyAvail.units)
forage$PreyAvail.units <- gsub('hectare', 'ha', 
                               forage$PreyAvail.units)
forage$PreyAvail.units <- trimws(forage$PreyAvail.units)
forage$PreyAvail.units <- gsub('  ', ' ', forage$PreyAvail.units)

sort(unique(forage[, c('PreyAvail.units')]))


# Standardize units of prey eaten
forage$PreyEaten.units <- tolower(forage$Original.y.units)
sort(unique(forage[, c('PreyEaten.units')]))

forage$PreyEaten.units <- gsub('pred per prey', 'prey eaten per pred', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('prey per', 'prey eaten per', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('prey parasitized', 'prey eaten', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('killed', 'eaten', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('attacks', 'eaten', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('consumed', 'eaten', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('consumed', 'eaten', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('parasitoid', 'pred', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('predator', 'pred', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('chl equiv', '', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('proportion of prey', 'proportion', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('proportion eaten', 'proportion prey eaten', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('percent of prey captured', 'proportion prey eaten', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('per capita prey deaths', 'proportion prey eaten', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('mg c per', 'mg c prey eaten per', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('ml per', 'ml prey eaten per', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('ng chl per', 'ng prey eaten per', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('um3 per g per', 'um3 prey eaten per g pred per', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('ug', 'µg', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('um', 'µm', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('ng per pred', 'ng prey eaten per pred', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub(', calculated', '', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('# of feeds/100 sec', 'prey eaten per 100 sec', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('individual', 'pred', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('during', 'per', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub(' days', ' day',
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub(' mo$', ' month',
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub(' s$', ' sec',
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub(' h$', ' hour',
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('hours', 'hour',
                               forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('daphnia', 'prey', 
                               forage$PreyEaten.units)
forage$PreyEaten.units <- trimws(forage$PreyEaten.units)
forage$PreyEaten.units <- gsub('  ', ' ', forage$PreyEaten.units)

sort(unique(forage[, c('PreyEaten.units')]))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Standardize original error to SE units ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
suppressWarnings(
  forage$Sample.size..per.density. <-
    as.numeric(forage$Sample.size..per.density.)
)

forage$Original.error.units[grep('95',forage$Original.error.units)] <- '95%CI'
forage$Original.error.units[grep('SE',forage$Original.error.units)] <- 'SE'
forage$PreyEaten.SE <- NA

sel <- forage$Rate.is=='mean' & forage$Original.error.units=='SE' | 
  forage$Rate.is=='mean' & forage$Original.error.units == ''
forage$PreyEaten.SE[sel] <- forage$Original.error[sel]

sel <- forage$Rate.is=='mean' & forage$Original.error.units=='95%CI'
forage$PreyEaten.SE[sel] <- forage$Original.error[sel] / 1.96

sel <- forage$Rate.is=='mean' & forage$Original.error.units=='SD'
forage$PreyEaten.SE[sel] <- forage$Original.error[sel] / 
  sqrt(forage$Sample.size..per.density.[sel])

sel <- forage$Rate.is=='mean' & forage$Original.error.units=='Range'
forage$PreyEaten.SE[sel] <- forage$Original.error[sel] / 2 # As assumed by DeLong & Uiterwaal

# If no error is given for mean study, treat as raw data study
sel <- forage$Rate.is == 'mean' & is.na(forage$Original.error) | 
  forage$Rate.is == 'mean' & forage$Original.error == ''
forage$Rate.is[sel] <- 'raw data'

# If not sample size is given for mean study, assume one
sel <- forage$Rate.is == 'mean' & is.na(forage$Sample.size..per.density.) | 
  forage$Rate.is == 'mean' & forage$Sample.size..per.density. == ''
forage$Sample.size..per.density.[sel] <- 3 # As assumed by DeLong & Uiterwaal


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Assume negative rates are digitization errors ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
forage$PreyEaten[forage$PreyEaten < 0] <- 0
forage$Foraging.rate[forage$Foraging.rate < 0] <- 0


# ~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~
# Standardize the meta-data -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~
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


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Standardize prey eaten to total prey eaten  ----
# (by all predators, not per predator)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# The next throws errors for datasets that don't have the correct predator abundances
# reported.  These datasets will be removed by "data_subset.R" as they mostly 
# represent entered predator count ranges rather than single counts.
# (Uiterwaal & DeLong used the mid-point of provided ranges.)

# Assume that studies with missing predator number reflect a single predator
# (but provided ranges are still NA)
sel <- forage_meta$Pred.per.arena == ''
forage_meta$Pred.per.arena[sel] <- 1

suppressWarnings(
  forage_meta$Pred.per.arena <- as.numeric(forage_meta$Pred.per.arena)
)

# merge predator counts into main data
forage <- merge(forage, 
                forage_meta[, c('Data.set', 'Pred.per.arena')], 
                all.x = TRUE)

pp <- grepl('per pair', forage$PreyEaten.units)
forage$Pred.per.arena[pp] <- 2
forage$PreyEaten.units <- gsub('per pair ', '', forage$PreyEaten.units)

sel <- grepl('per(.*)pred', forage$PreyEaten.units)
forage$PreyEaten[sel] <- 
  forage$PreyEaten[sel] * forage$Pred.per.arena[sel]
forage$PreyEaten.SE[sel] <- 
  forage$PreyEaten.SE[sel] * forage$PreyEaten.SE[sel]
forage$PreyEaten.units[sel] <- 
  gsub(' per(.*)pred', '', forage$PreyEaten.units[sel])
forage$PreyEaten.units <- paste('total', forage$PreyEaten.units)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Some prey available are given per predator ----
# Convert these to total available prey
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sel <- grep('per predator', forage$PreyAvail.units)
forage$PreyAvail[sel] <- 
  forage$PreyAvail[sel] * forage$Pred.per.arena[sel] 
forage$PreyAvail.units[sel] <- 
  gsub(' per predator', '', forage$PreyAvail.units[sel])


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Drop treatments that have zero prey offered ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# These shouldn't actually exist.
# They are data-formatting issues in the original data file
# that should be fixed in FoRAGE v4.

forage$PreyAvail <- as.numeric(forage$PreyAvail)
keep <- forage$PreyAvail != 0
forage <- forage[keep,]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert 'proportion eaten' to count of eaten ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sel2 <- grepl('proportion', forage$PreyEaten.units)
forage[sel2,
       c('Data.set','Source','PreyAvail','PreyEaten','PreyEaten.SE',
         'PreyEaten.units', 'Rate.is')]

# assume these are actually counts (have already been converted)
sel <- forage$Data.set %in% c(1634:1639)
forage$PreyEaten.units[sel] <- 'total prey eaten'

# assume these are correct percentages (raw data study)
sel <- forage$Data.set %in% 1882
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel] * forage$PreyAvail[sel])
forage$PreyEaten.units[sel] <- 'total prey eaten'

# assume these are correct percentages (mean study)
sel <- forage$Data.set %in% c(2195:2196, 2365:2368, 2386:2387)
forage$PreyEaten[sel] <- forage$PreyEaten[sel] * forage$PreyAvail[sel]
forage$PreyEaten.SE[sel] <- forage$PreyEaten.SE[sel] * forage$PreyAvail[sel]
forage$PreyEaten.units[sel] <- 'total prey eaten'


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dataset/study-specific fixes -- SPACE and TIME ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# We desire to use binomial and Poisson likelihoods,
# thus require integer-valued prey available counts for all studies
# and integer-valued prey eaten counts for 'raw data' studies.
# We rescale these for specific datasets where needed. 

study <- 'Gilg et al 2006'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- forage$PreyAvail[sel] * 1000
forage$PreyAvail.units[sel] <- 'prey per 1000 ha'

study <- 'Farazmand and Amir-Maafi 2021' # appears to be digitization precision
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel])
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel])

study <- 'Juliano et al 2022'  # appears to be digitization precision
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel])
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel])

study <- 'Quinn et al 2003'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- forage$PreyAvail[sel] * 1000
forage$PreyEaten[sel] <- forage$PreyEaten[sel] * 1000
forage$PreyAvail.units[sel] <- gsub('1000 ', '', forage$PreyAvail.units[sel])
forage$PreyEaten.units[sel] <- gsub('1000 ', '', forage$PreyEaten.units[sel])

study <- 'Vucic-Pestic et al. 2010'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- 10^forage$PreyAvail[sel]
forage$PreyEaten[sel] <- round(10^forage$PreyEaten[sel] - 1)
forage$PreyAvail.units[sel] <- 'prey'
forage$PreyEaten.units[sel] <- 'total prey eaten'

study <- 'Libourel Houde and Roman 1987'
sel <- grep(study, forage$Source)
forage$PreyEaten.units[sel] <- gsub('10(\\^)3 ', '', forage$PreyEaten.units[sel] )

study <- 2458
sel <- grep(study, forage$Data.set)
forage$PreyEaten[sel] <- forage$PreyEaten[sel]  / (100*24)
forage$Trial.duration..h.[sel] <- 1
forage$PreyEaten.units[sel] <- gsub('per 100 day', '', forage$PreyEaten.units[sel] )

study <- 'Nandini and Sarma 1999'
sel <- grep(study, forage$Source)
forage$PreyEaten.units[sel] <- gsub('per 0.5', '', forage$PreyEaten.units[sel] )
forage$PreyAvail[sel] <- forage$PreyAvail[sel] * 20
forage$PreyAvail.units[sel] <- gsub('per ml', '', forage$PreyAvail.units[sel] )

study <- 'Bryan et al 1995'
sel <- grep(study, forage$Source)
forage$Trial.duration..h.[sel] <- (30/60)/60
forage$PreyEaten.units[sel] <- gsub('per 30 sec', '', forage$PreyEaten.units[sel] )
# 
# study <- 'Sandheinrich and Atchison 1989'
# sel <- grepl(study, forage$Source) & grepl('per 30 sec', forage$PreyEaten.units)
# forage$Trial.duration..h.[sel] <- (30/60)/60
# forage$PreyEaten.units[sel] <- gsub('per 30 sec', '', forage$PreyEaten.units[sel] )
# forage$PreyAvail[sel] <- forage$PreyAvail[sel] * 60
# forage$PreyAvail.units[sel] <- gsub('per l', '', forage$PreyAvail.units[sel] )
# 
# study <- 'Sandheinrich and Atchison 1989'
# sel <- grepl(study, forage$Source) & grepl('per 3 min', forage$PreyEaten.units)
# forage$Trial.duration..h.[sel] <- 3/60
# forage$PreyEaten.units[sel] <- gsub('per 3 min', '', forage$PreyEaten.units[sel] )
# forage$PreyAvail[sel] <- forage$PreyAvail[sel] * 60
# forage$PreyAvail.units[sel] <- gsub('per l', '', forage$PreyAvail.units[sel] )


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Match-up units of prey available and prey eaten -- TIME ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
forage$Trial.duration..h. <- as.numeric(forage$Trial.duration..h.)
forage$PreyAvail.units <- trimws(forage$PreyAvail.units)
forage$PreyEaten.units <- trimws(forage$PreyEaten.units)

# Inspect unit combinations
u <- unique(forage[, c('PreyAvail.units', 'PreyEaten.units', 'Trial.duration..h.')])
u <- u[ do.call(order, u), ]
u



isNA <- is.na(forage$Trial.duration..h.)

sel <- isNA & grepl('per hour', forage$PreyEaten.units)
forage$Trial.duration..h.[sel] <- 1
forage$PreyEaten.units[sel] <- gsub(' per hour', '', forage$PreyEaten.units[sel])

sel <- isNA & grepl('per day', forage$PreyEaten.units)
forage$Trial.duration..h.[sel] <- 24
forage$PreyEaten.units[sel] <- gsub(' per day', '', forage$PreyEaten.units[sel])

sel <- isNA & grepl('per month', forage$PreyEaten.units)
forage$Trial.duration..h.[sel] <- 24 * 30
forage$PreyEaten.units[sel] <- gsub(' per month', '', forage$PreyEaten.units[sel])

sel <- isNA & grepl('per year', forage$PreyEaten.units)
forage$Trial.duration..h.[sel] <- 24 * 365
forage$PreyEaten.units[sel] <- gsub(' per year', '', forage$PreyEaten.units[sel])

# sel <- isNA & grepl('per min', forage$PreyEaten.units)
# forage$Trial.duration..h.[sel] <- 1/60
# forage$PreyEaten.units[sel] <- gsub(' per min', '', forage$PreyEaten.units[sel])
# 
# sel <- isNA & grepl('per sec', forage$PreyEaten.units)
# forage$Trial.duration..h.[sel] <- 1/60/60
# forage$PreyEaten.units[sel] <- gsub(' per sec', '', forage$PreyEaten.units[sel])
# 
# sel <- isNA & grepl('per 100 sec', forage$PreyEaten.units)
# forage$Trial.duration..h.[sel] <- 100*1/60/60
# forage$PreyEaten.units[sel] <- gsub(' per year', '', forage$PreyEaten.units[sel])


sel <- isNA &
  forage$PreyEaten.units == 'total mg prey eaten' & 
    forage$PreyAvail.units == 'mg prey'
forage$Trial.duration..h.[sel] <- 1

sel <- isNA &
  forage$PreyEaten.units == 'total prey eaten' & 
  forage$PreyAvail.units == 'prey'
forage$Trial.duration..h.[sel] <- 1

sel <- isNA &
  forage$PreyEaten.units == 'total prey eaten' & 
  forage$PreyAvail.units == 'prey per cm2'
forage$Trial.duration..h.[sel] <- 1

sel <- isNA &
  forage$PreyEaten.units == 'total prey eaten' & 
  forage$PreyAvail.units == 'prey per l'
forage$Trial.duration..h.[sel] <- 1

sel <- isNA &
  forage$PreyEaten.units == 'total prey eaten' & 
  forage$PreyAvail.units == 'prey per ml'
forage$Trial.duration..h.[sel] <- 1

sel <- isNA &
  forage$PreyEaten.units == 'total prey eaten per breeding season' & 
  forage$PreyAvail.units == 'prey per ha'
forage$Trial.duration..h.[sel] <- 1
forage$PreyEaten.units[sel] <- 'total prey eaten'

sel <- grepl('per hour', forage$PreyEaten.units) & forage$Trial.duration..h. == 1
forage$PreyEaten[sel] <- forage$PreyEaten[sel] * forage$Trial.duration..h.[sel]
forage$PreyEaten.SE[sel] <- forage$PreyEaten.SE[sel] * forage$Trial.duration..h.[sel]
forage$PreyEaten.units[sel] <- gsub(' per hour', '', forage$PreyEaten.units[sel])

# sel <- grepl('per day', forage$PreyEaten.units) & forage$Trial.duration..h. == 24
# forage$PreyEaten[sel] <- forage$PreyEaten[sel] * forage$Trial.duration..h.[sel]
# forage$PreyEaten.SE[sel] <- forage$PreyEaten.SE[sel] * forage$Trial.duration..h.
# forage$PreyEaten.units[sel] <- gsub(' per day', '', forage$PreyEaten.units[sel])


# If time information is still missing, assume unit time
isNA <- is.na(forage$Trial.duration..h.)
forage$Trial.duration..h.[isNA] <- 1

# Non-integer prey eatn ---- Quick fix by brute force
# if any "raw data" consumption is reported as non-integer, rescale it to integer
message('Rescaling prey eaten as needed.')
forage_meta$PreyEaten.Rescaled <- FALSE
forage_meta$PreyEaten.RescalingFactor <- NA

datasetIDs <- unique(forage$Data.set)
pb <- txtProgressBar(min = 0,
                     max = length(datasetIDs))
b <- 1
for (i in 1:length(datasetIDs)){
  sel <- forage$Data.set == datasetIDs[i]
  sel.meta <- forage_meta$Data.set == datasetIDs[i]

  if(unique(forage$Rate.is[sel]) == 'raw data' & 
     all(!is.na(forage$PreyEaten[sel]))){ # These come from NAs for predator abundance
    if(any(!is.wholenumber(forage$PreyEaten[sel]))) {
      scale <- 10^as.numeric(max(nchar(gsub('^*.*\\.', '', forage$PreyEaten[sel]))))
      forage$PreyEaten[sel] <- forage$PreyEaten[sel] * scale
      
      # forage$Rate.is[sel] <- 'mean'
      # forage$PreyEaten.SE[sel] <- 0
      # forage$Sample.size..per.density.[sel] <- 1
      
      forage_meta$PreyEaten.Rescaled[sel.meta] <- TRUE
      forage_meta$PreyEaten.RescalingFactor[sel.meta] <- scale
  }}
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Match-up units of prey available and prey eaten -- SPACE ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Inspect unit combinations
u <- unique(forage[, c('PreyAvail.units', 'PreyEaten.units')])
u[ do.call(order, u), ]

u <- unique(forage[, c('PreyAvail.units', 'PreyEaten.units', 'X2D.Arena..units')])
u[ do.call(order, u), ]

u <- unique(forage[, c('PreyAvail.units', 'PreyEaten.units', 'X3D.Arena..units')])
u[ do.call(order, u), ]

# Too many prey eaten ---- Quick fix by brute force
# For non-replacement datasets where the number of eaten prey exceeds 
# the number of available prey, we rescale the number of prey available.

message('Rescaling prey abundances as needed.')
forage_meta$PreyAvail.Rescaled <- FALSE
forage_meta$PreyAvail.RescalingFactor <- NA

datasetIDs <- unique(forage$Data.set)
pb <- txtProgressBar(min = 0,
                     max = length(datasetIDs))
b <- 1
for (i in 1:length(datasetIDs)){
  sel <- forage$Data.set == datasetIDs[i]
  sel.meta <- forage_meta$Data.set == datasetIDs[i]

  if(forage_meta$Prey.replaced.[sel.meta] == FALSE & 
     all(!is.na(forage$PreyEaten[sel]))){ # These come from NAs for predator abundance
        if(any(forage$PreyEaten[sel] > forage$PreyAvail[sel])){
          scale <- ceiling(max(forage$PreyEaten[sel] / forage$PreyAvail[sel]))
          forage$PreyAvail[sel] <- scale * forage$PreyAvail[sel] 
          forage_meta$PreyAvail.Rescaled[sel.meta] <- TRUE
          forage_meta$PreyAvail.RescalingFactor[sel.meta] <- scale
    }
  }
  b <- b + 1
  setTxtProgressBar(pb, b)
}
close(pb)

# ~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~
# Data-specific fixes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~

# study <- 'Monteleone & Duguay 1988'
# sel <- grep(study, forage$Source)
# forage$PreyAvail[sel] <- forage$PreyAvail[sel] * 15
# forage$PreyAvail.units[sel] <- 'prey'
# 
# study <- 'Holling 1959'
# sel <- grepl(study, forage$Source) & forage$Rate.is == 'mean'
# forage$PreyAvail[sel] <- forage$PreyAvail[sel] * 40
# forage$PreyAvail.units[sel] <- 'prey'
# 
# study <- 'Dumont et al 1994'
# sel <- grepl(study, forage$Source)
# 
# study <- 'Båmstedt et al 1994'
# sel <- grepl(study, forage$Source)
# forage$PreyAvail[sel] <- forage$PreyAvail[sel] * 40
# forage$PreyAvail.units[sel] <- 'prey'


# forage[sel, ]

# ~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~
# Assess for units of mass ----
# ~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~
# Are measures of prey abundance or prey eaten made in units of mass?
temp <- unique(forage[, c('Data.set', 'PreyAvail.units', 'PreyEaten.units')])

temp$mass.study <- !grepl('proportion', temp$PreyEaten.units) &
  !(grepl('^prey', temp$PreyAvail.units) |
      grepl('^total prey', temp$PreyEaten.units))

forage_meta <- merge(forage_meta, 
                     temp[, c('Data.set','mass.study')], 
                     all.x = TRUE)

#########################
#########################
# Cross-check dataset IDs ----
#########################
#########################

# As a consequence of the dropped zero treatments, 
# some datasets are dropped.
# These are thus no longer present in both sheets and must be
# removed from the other
keep <- unique(forage$Data.set) %in% unique(forage_meta$Data.set)
forage <- forage[keep,]
keep <- unique(forage_meta$Data.set) %in% unique(forage$Data.set)
forage_meta <- forage_meta[keep,]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Proceed with packaging the data! ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

datasetIDs <- unique(forage$Data.set)
datasets <- vector(mode = 'list', 
                   length = length(datasetIDs))

pb <- txtProgressBar(min = 0,
                     max = length(datasetIDs))
b <- 1

for (i in 1:length(datasetIDs)) {
  temp.study <- subset(forage, forage$Data.set == datasetIDs[i])
  temp.info <- subset(forage_meta, forage_meta$Data.set == datasetIDs[i])
  
  datasetName = paste(datasetIDs[i], unique(temp.study$AuthorYear), sep='-')
  
  study.info <- list(
    datasetID = datasetIDs[i],
    datasetName = datasetName,
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
    pred.per.arena = unique(temp.info$Pred.per.arena),
    mass.study = temp.info$mass.study,
    rescaled.eaten = temp.info$PreyEaten.Rescaled,
    rescaled.eaten.scaling.factor = temp.info$PreyEaten.RescalingFactor,
    rescaled.prey = temp.info$PreyAvail.Rescaled,
    rescaled.prey.scaling.factor = temp.info$PreyAvail.RescalingFactor
  )
  
  if (any(lapply(study.info, length) > 1)) {
    stop(paste('Dataset ', datasetName, 
               ' has at least one non-unique attribute.'))
    print(study.info)
  }
  
  data.orig <- temp.study[,
                          c(
                            'Pred.per.arena',
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
                            'Standard.Error'
                          )]
  
  x <- temp.study$PreyAvail
  y <- temp.study$PreyEaten
  se <- temp.study$PreyEaten.SE
  
  t <- temp.study$Trial.duration..h.
  n <- temp.study$Sample.size..per.density.
  p <- temp.study$Pred.per.arena
  
  data <- data.frame(
    Nprey = x,
    Npredator = p,
    Nconsumed = y,
    Nconsumed.se = se,
    n = n,
    Time = t
  )
  
  if(study.info$type == 'mean') {
    colnames(data)[3] <- 'Nconsumed.mean'
  } else{
    data <- data[, -c(4, 5)] # remove 'se' and 'n' from raw data studies
  }
  
  
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
