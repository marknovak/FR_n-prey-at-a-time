########################################################
# Clean-up, organize, and save FoRAGE datasets to a list
########################################################
# options(warn = 1)
rm(list = ls())

# Convenience functions
is.wholenumber <- function(x, tol = .Machine$double.eps^0.2){
  abs(x - round(x)) < tol
  }

my.signif <- function(x, digits = 2){
  round(x, max(0, digits - ceiling(max(log10(x)))))
}

maxdecimalplaces <- function(x){
  dp <- (x %% 1) != 0
  if(any(dp)){
    x <- sub('0+$', '', as.character(x[dp]))
    x <- sub(".*\\.", "", x)
    return(max(nchar(x)))
  } else {
    return(0)
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# forage_meta <- read.csv('../../data/FoRAGE_db_V3_Jan_2_2023.csv')
# forage <-
#   read.csv('../../data/FoRAGE_db_V3_Jan_2_2023_original_curves.csv')
forage_meta <- read.csv('../../data/FoRAGE_db_V4_Sept_27_2023_sources_and_meta.csv')
forage <-
  read.csv('../../data/FoRAGE_db_V4_Sept_27_2023_original_curves.csv', 
           header=TRUE, 
           stringsAsFactors=FALSE,
           fileEncoding="latin1")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~
# Basic cleanup ----
# ~~~~~~~~~~~~~
colnames(forage)[1] <- 'Data.set'
colnames(forage)[3] <- 'Predator'

# Remove extra columns
sel <- !grepl('X.\\d{1,2}$', colnames(forage))
forage <- forage[, sel]

# Remove extra rows
sel <- !is.na(forage$Data.set)
forage <- forage[sel ,]

sel <- !is.na(forage_meta$Data.set)
forage_meta <- forage_meta[sel ,]

# Fix variable types - the introduced NAs should get fixed in the original
# Some are fixed below.
forage$Original.y <- as.numeric(forage$Original.y)
forage$Sample.size..per.density. <- as.numeric(forage$Sample.size..per.density.)
forage$X2D.Arena.size <- as.numeric(forage$X2D.Arena.size)
forage$Foraging.rate <- as.numeric(forage$Foraging.rate)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dataset-specific typo fixes & digitalization precision errors ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# forage[sel, ]

sel <- forage$Data.set == 2887 & forage$Original.x == 10
forage$Original.y[sel] <- 89
forage$Foraging.rate[sel] <- 89

sel <- forage$Data.set %in% 62:67
forage$Sample.size..per.density.[sel] <- 8

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

sel <- forage$Data.set == 762 & forage$Original.x == 0
forage$Original.error[sel] <- 0

sel <- forage$Data.set %in% 2087
forage$Original.y[sel] <- round(forage$Original.y[sel], 2)

sel <- forage$Data.set %in% 189
forage$Original.y[sel] <- round(forage$Original.y[sel], 2)
forage$Original.x[sel] <- round(forage$Original.x[sel], 3)

sel <- forage$Data.set %in% 553
forage$Original.x[sel] <- round(forage$Original.x[sel], 3)

sel <- forage$Data.set %in% 545
forage$Original.y[sel] <- round(forage$Original.y[sel], 2)

sel <- forage$Data.set %in% 2168:2171
forage$Original.y[sel] <- round(forage$Original.y[sel])

sel <- forage$Data.set %in% 143:144
forage$Original.x[sel] <- round(forage$Original.x[sel])
forage$Original.y[sel] <- round(forage$Original.y[sel], 1)

sel <- forage$Data.set %in% 192:194
forage$Original.x[sel] <- round(forage$Original.x[sel])
forage$Original.y[sel] <- round(forage$Original.y[sel])

sel <- forage$Data.set %in% 280:283
forage$Original.x[sel] <- round(forage$Original.x[sel])
forage$Original.y[sel] <- round(forage$Original.y[sel])

sel <- forage$Data.set %in% 517:521
forage$Original.y[sel] <- round(forage$Original.y[sel], 1)

sel <- forage$Data.set %in% 853
forage$Original.x[sel] <- round(forage$Original.x[sel], 2)
forage$Original.y[sel] <- round(forage$Original.y[sel], 1)

sel <- forage$Data.set %in% 191
forage$Original.x[sel] <- round(forage$Original.x[sel], 2)
forage$Original.y[sel] <- round(forage$Original.y[sel], 1)

sel <- forage$Data.set %in% 484:487
forage$Original.x[sel] <- round(forage$Original.x[sel])
forage$Original.y[sel] <- round(forage$Original.y[sel], 2)

sel <- forage$Data.set %in% 974:985
forage$Original.x[sel] <- round(forage$Original.x[sel])

sel <- forage$Data.set %in% 2250
forage$Original.x[sel] <- round(forage$Original.x[sel])
forage$Original.y[sel] <- round(forage$Original.y[sel], 1)

sel <- forage$Data.set %in% 208:227
forage$Original.y[sel] <- round(forage$Original.y[sel])

sel <- forage$Data.set %in% 488:491
forage$Original.x[sel] <- round(forage$Original.x[sel], 2)
forage$Original.y[sel] <- round(forage$Original.y[sel], 1)

sel <- forage$Data.set %in% 1416:1421
forage$Original.x[sel] <- round(forage$Original.x[sel])
forage$Original.y[sel] <- round(forage$Original.y[sel])

sel <- forage$Data.set %in% 2861:2862
forage$Original.x[sel] <- round(forage$Original.x[sel])
forage$Original.y[sel] <- round(forage$Original.y[sel], 1)

sel <- forage$Data.set %in% 707:719
forage$Original.y[sel] <- round(forage$Original.y[sel], 3)

sel <- forage$Data.set %in% 31:33
forage$Original.x[sel] <- round(forage$Original.x[sel])
forage$Original.y[sel] <- round(forage$Original.y[sel])

sel <- forage$Data.set %in% 31:33
forage$Original.x[sel] <- round(forage$Original.x[sel], 1)
forage$Original.y[sel] <- round(forage$Original.y[sel], 1)

sel <- forage$Data.set %in% 1681:1692
forage$Original.x[sel] <- round(forage$Original.x[sel])
forage$Original.y[sel] <- round(forage$Original.y[sel])

sel <- forage$Data.set %in% 1834:1836
forage$Original.x[sel] <- round(forage$Original.x[sel])
forage$Original.y[sel] <- round(forage$Original.y[sel], 1)

sel <- forage$Data.set %in% 1837:1841
forage$Original.x[sel] <- round(forage$Original.x[sel])
forage$Original.y[sel] <- round(forage$Original.y[sel])

sel <- forage$Data.set %in% 1837:1838
forage$Original.x[sel] <- round(forage$Original.x[sel])
forage$Original.y[sel] <- round(forage$Original.y[sel])

sel <- forage$Data.set %in% 1839:1841
forage$Original.x[sel] <- round(forage$Original.x[sel])
forage$Original.y[sel] <- round(forage$Original.y[sel])

sel <- forage$Data.set %in% 1842:1843
forage$Original.x[sel] <- round(forage$Original.x[sel])
forage$Original.y[sel] <- round(forage$Original.y[sel], 1)

sel <- forage$Data.set %in% 1844:1845
forage$Original.x[sel] <- round(forage$Original.x[sel])
forage$Original.y[sel] <- round(forage$Original.y[sel], 1)

sel <- forage$Data.set %in% 1846:1850
forage$Original.x[sel] <- round(forage$Original.x[sel])

sel <- forage$Data.set %in% 1883:1885
forage$Original.x[sel] <- round(forage$Original.x[sel], 1)
forage$Original.y[sel] <- round(forage$Original.y[sel], 1)

sel <- forage$Data.set %in% 1906:1907
forage$Original.x[sel] <- round(forage$Original.x[sel], 1)
forage$Original.y[sel] <- round(forage$Original.y[sel], 1)

sel <- forage$Data.set %in% 1908:1909
forage$Original.x[sel] <- round(forage$Original.x[sel], 1)
forage$Original.y[sel] <- round(forage$Original.y[sel], 1)

sel <- forage$Data.set %in% 1939:1947
forage$Original.x[sel] <- round(forage$Original.x[sel], 2)
forage$Original.y[sel] <- round(forage$Original.y[sel], 1)

sel <- forage$Data.set %in% 2084:2089
forage$Original.x[sel] <- round(forage$Original.x[sel])
forage$Original.y[sel] <- round(forage$Original.y[sel])

sel <- forage$Data.set %in% 2322:2332
forage$Original.x[sel] <- round(forage$Original.x[sel], 1)
forage$Original.y[sel] <- round(forage$Original.y[sel], 3)

sel <- forage$Data.set %in% 2327
forage$Original.x[sel] <- round(forage$Original.x[sel])

sel <- forage$Data.set %in% 2343:2346
forage$Original.x[sel] <- round(forage$Original.x[sel], 3)
forage$Original.y[sel] <- round(forage$Original.y[sel], 2)

sel <- forage$Data.set %in% 2377:2378
forage$Original.x[sel] <- round(forage$Original.x[sel])
forage$Original.y[sel] <- round(forage$Original.y[sel])

sel <- forage$Data.set %in% 2379:2385
forage$Original.x[sel] <- round(forage$Original.x[sel])

sel <- forage$Data.set %in% 2388:2403
forage$Original.x[sel] <- round(forage$Original.x[sel])

sel <- forage$Data.set %in% 1973:1978
forage$Original.x[sel] <- round(forage$Original.x[sel], 1)
forage$Original.y[sel] <- round(forage$Original.y[sel], 1)


# sel <- forage$Data.set %in% 990


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
# unique(forage[, c('X2D.Arena..units', 'X3D.Arena..units')])


# Create new x and y on which to affect changes
forage$PreyAvail <- forage$Original.x
forage$PreyEaten <- forage$Original.y


# Standardize units of prey available
# sort(unique(forage[, c('Original.x.units')]))
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

# sort(unique(forage[, c('PreyAvail.units')]))


# Standardize units of prey eaten
forage$PreyEaten.units <- tolower(forage$Original.y.units)
# sort(unique(forage[, c('PreyEaten.units')]))

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

# sort(unique(forage[, c('PreyEaten.units')]))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Standardize original error to SE units ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# If no sample size is given for mean study, assume that it is...
sel <- forage$Rate.is == 'mean' & is.na(forage$Sample.size..per.density.) | 
  forage$Rate.is == 'mean' & forage$Sample.size..per.density. == ''
forage$Sample.size..per.density.[sel] <- 3 # As assumed by DeLong & Uiterwaal


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

sel <- forage$Rate.is=='mean' & forage$Original.error == 0 & !is.na(forage$Original.error)
forage$PreyEaten.SE[sel] <- forage$Original.error[sel]

# If no error is given for mean study, we infer it based on the 
# global relationship (see below)

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


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Some prey available and eaten are given per predator ----
# Convert these to total available prey
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sel <- grepl('per pair', forage$PreyEaten.units)
forage$Pred.per.arena[sel] <- 2
forage$PreyEaten.units <- gsub('per pair ', '', forage$PreyEaten.units)

sel <- grepl('per(.*)pred', forage$PreyEaten.units)
forage$PreyEaten[sel] <- 
  forage$PreyEaten[sel] * forage$Pred.per.arena[sel]
forage$PreyEaten.SE[sel] <- 
  forage$PreyEaten.SE[sel] * forage$Pred.per.arena[sel]
forage$PreyEaten.units[sel] <- 
  gsub(' per(.*)pred', '', forage$PreyEaten.units[sel])
forage$PreyEaten.units <- paste('total', forage$PreyEaten.units)

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
forage$PreyAvail <- as.numeric(forage$PreyAvail)
sel <- forage$PreyAvail != 0
forage <- forage[sel,]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert 'proportion eaten' to count of eaten ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sel2 <- grepl('proportion', forage$PreyEaten.units)
# forage[sel2,
#        c('Data.set','Source','PreyAvail','PreyEaten','PreyEaten.SE',
#          'PreyEaten.units', 'Rate.is')]

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

study <- 'Hewett 1980'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 1000/15)
forage$PreyAvail.units[sel] <- 'prey per l'
forage$PreyEaten[sel] <- forage$PreyEaten[sel] * 24
forage$PreyEaten.SE[sel] <-forage$PreyEaten.SE[sel] *24
forage$PreyEaten.units[sel] <- 'total prey eaten' 

study <- 'Gilg et al 2006'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- forage$PreyAvail[sel] * 1000
forage$PreyAvail.units[sel] <- 'prey per 1000 ha'

study <- 'Farazmand and Amir-Maafi 2021' # appears to be digitization precision
sel <- grep(study, forage$Source)
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel])

study <- 'Juliano et al 2022'  # appears to be digitization precision
sel <- grep(study, forage$Source)
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel])

study <- 'Quinn et al 2003'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- forage$PreyAvail[sel] * 1000
forage$PreyEaten[sel] <- forage$PreyEaten[sel] * 1000
forage$PreyAvail.units[sel] <- gsub('1000 ', '', forage$PreyAvail.units[sel])
forage$PreyEaten.units[sel] <- gsub('1000 ', '', forage$PreyEaten.units[sel])

study <- 'Vucic-Pestic et al. 2010'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(10^forage$PreyAvail[sel])
forage$PreyEaten[sel] <- round(10^forage$PreyEaten[sel] - 1)
forage$PreyAvail.units[sel] <- 'prey'
forage$PreyEaten.units[sel] <- 'total prey eaten'

study <- 'Libourel Houde and Roman 1987'
sel <- grep(study, forage$Source)
forage$PreyEaten.units[sel] <- gsub('10(\\^)3 ', '', forage$PreyEaten.units[sel] )
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel])

study <- 'Moleón et al 2012'
sel <- grep(study, forage$Source)
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel]  / (100*24), 4)
forage$Trial.duration..h.[sel] <- 1
forage$PreyEaten.units[sel] <- gsub('per 100 day', '', forage$PreyEaten.units[sel] )
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 1000)
forage$PreyAvail.units[sel] <- 'prey per 1000 ha'

study <- 'Nandini and Sarma 1999'
sel <- grep(study, forage$Source)
forage$PreyEaten.units[sel] <- gsub('per 0.5', '', forage$PreyEaten.units[sel] )
forage$PreyAvail[sel] <- forage$PreyAvail[sel] * 20
forage$PreyAvail.units[sel] <- gsub('per ml', '', forage$PreyAvail.units[sel] )

study <- 'Bryan et al 1995'
sel <- grep(study, forage$Source)
forage$Trial.duration..h.[sel] <- (30/60)/60
forage$PreyEaten.units[sel] <- gsub('per 30 sec', '', forage$PreyEaten.units[sel] )

study <- 'De Villemereuil and López-Sepulcre 2011'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 10, 0)
forage$PreyAvail.units[sel] <- gsub('per l', '', forage$PreyAvail.units[sel] )
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel], 0)

study <- 'Dale et al 1994'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- forage$PreyAvail[sel] * 1000
forage$PreyAvail.units[sel] <- gsub('per km2', 'per 1000 km2', forage$PreyAvail.units[sel] )

study <- 'Twardochleb et al 2012'
sel <- grep(study, forage$Source)
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel] * 30)
forage$PreyEaten.units[sel] <- gsub('per min', '', forage$PreyEaten.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Moss et al 2007'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel]) * 50
forage$PreyAvail.units[sel] <- gsub('per l', '', forage$PreyAvail.units[sel] )
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel] * 60)
forage$PreyEaten.units[sel] <- gsub('per min', '', forage$PreyEaten.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Michalko and Pekar 2017 AM NAT'
sel <- grep(study, forage$Source)
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel] * 7)
forage$PreyEaten.units[sel] <- gsub('per hour', '', forage$PreyEaten.units[sel] )

study <- 'Haskell et al 2017'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 60)
forage$PreyAvail.units[sel] <- gsub('per ml', '', forage$PreyAvail.units[sel] )
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel] * 10 )
forage$PreyEaten.units[sel] <- gsub('per min', 'per 10 min', forage$PreyEaten.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Holling 1965'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 6902)
forage$PreyAvail.units[sel] <- gsub('per cm2', '', forage$PreyAvail.units[sel] )

study <- 'Witz 1996'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 4)

study <- 'Munk 1995'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 172)
forage$PreyAvail.units[sel] <- gsub('per l', '', forage$PreyAvail.units[sel] )
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel] * 60 * 24)
forage$PreyEaten.units[sel] <- gsub('per min', '', forage$PreyEaten.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Duijns et al 2015'
sel <- grep(study, forage$Source)
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel] * 60 * 5, 1)
forage$PreyEaten.units[sel] <- gsub('per sec', 'per 5 min', forage$PreyEaten.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Stillman and Simmons 2006'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 16 * 3)
forage$PreyAvail.units[sel] <- gsub('per m2', '', forage$PreyAvail.units[sel] )
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel] * 60)
forage$PreyEaten.units[sel] <- gsub('per sec', 'per min', forage$PreyEaten.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Titelman & Hansson 2006'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 27)
forage$PreyAvail.units[sel] <- gsub('per l', '', forage$PreyAvail.units[sel] )
forage$PreyEaten.units[sel] <- gsub('per hour', '', forage$PreyEaten.units[sel] )

study <- 'Monteleone & Duguay 1988'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 15)
forage$PreyAvail.units[sel] <- gsub('per l', '', forage$PreyAvail.units[sel] )

study <- 'Saiz and Kiørboe 1995'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 2300)
forage$PreyAvail.units[sel] <- gsub('per ml', '', forage$PreyAvail.units[sel] )
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel] / 100)
forage$Trial.duration..h.[sel] <- 1

study <- 'Frost 1972'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 3500)
forage$PreyAvail.units[sel] <- gsub('per ml', '', forage$PreyAvail.units[sel] )
forage$PreyEaten.units[sel] <- gsub('per hour', '', forage$PreyEaten.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Miller et al 1992'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 20)
forage$PreyAvail.units[sel] <- gsub('per l', '', forage$PreyAvail.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Ohlberger et al 2008'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 80)
forage$PreyAvail.units[sel] <- gsub('per l', '', forage$PreyAvail.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Wickham 1995'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 30)
forage$PreyAvail.units[sel] <- gsub('per ml', '', forage$PreyAvail.units[sel] )
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel])
forage$PreyEaten.units[sel] <- gsub('per hour', '', forage$PreyEaten.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Lampert 1994'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 300)
forage$PreyAvail.units[sel] <- gsub('per l', '', forage$PreyAvail.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Rothhaupt 1990'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 20 * 1000)
forage$PreyAvail.units[sel] <- gsub('per l', '', forage$PreyAvail.units[sel] )
forage$PreyAvail.units[sel] <- gsub('mg', 'ng', forage$PreyAvail.units[sel] )
forage$PreyEaten.units[sel] <- gsub('per hour', '', forage$PreyEaten.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Seale and Beckvar 1980'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 150 / 1000)
forage$PreyAvail.units[sel] <- gsub('per ml', '', forage$PreyAvail.units[sel] )
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel] / 10^6)
forage$PreyEaten.units[sel] <- gsub('per hour', '', forage$PreyEaten.units[sel] )
forage$PreyEaten.SE[sel] <- round(forage$PreyEaten.SE[sel] / 10^6)
forage$Trial.duration..h.[sel] <- 1

study <- 'West and Post 2016'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 80)
forage$PreyAvail.units[sel] <- gsub('per l', '', forage$PreyAvail.units[sel] )
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel] * 100)
forage$Trial.duration..h.[sel] <- 1

study <- 'Jeong 1994'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 500)
forage$PreyAvail.units[sel] <- gsub('per ml', '', forage$PreyAvail.units[sel] )
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel], 2)
forage$PreyEaten.units[sel] <- gsub('per hour', '', forage$PreyEaten.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Uszko et al 2015'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 200 * 4)
forage$PreyAvail.units[sel] <- gsub('per ml', '', forage$PreyAvail.units[sel] )
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel] * 100)
forage$PreyEaten.units[sel] <- gsub('per hour', '', forage$PreyEaten.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Gismervik and Andersen 1997'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 500)
forage$PreyAvail.units[sel] <- gsub('per ml', '', forage$PreyAvail.units[sel] )
forage$PreyEaten.units[sel] <- gsub('per day', '', forage$PreyEaten.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Fedorenko 1975'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 10)
forage$PreyAvail.units[sel] <- gsub('per l', '', forage$PreyAvail.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Olesin et al 1994'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 460)
forage$PreyAvail.units[sel] <- gsub('per l', '', forage$PreyAvail.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Helgen 1987'
sel <- grep(study, forage$Source)
forage$PreyAvail.units[sel] <- gsub('per l', '', forage$PreyAvail.units[sel] )
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel] / 1000)
forage$Trial.duration..h.[sel] <- 1

study <- 'De Figueiredo et al 2007'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 1000)
forage$PreyAvail.units[sel] <- gsub('per l', '', forage$PreyAvail.units[sel] )
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel] * 3)
forage$PreyEaten.units[sel] <- gsub('per hour', '', forage$PreyEaten.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Rigler 1961'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 250)
forage$PreyAvail.units[sel] <- gsub('per ml', '', forage$PreyAvail.units[sel] )
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel] * 3 / 24)
forage$PreyEaten.units[sel] <- gsub('per hour', '', forage$PreyEaten.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Uye 1976'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 500)
forage$PreyAvail.units[sel] <- gsub('per ml', '', forage$PreyAvail.units[sel] )
forage$PreyEaten.units[sel] <- gsub('per hour', '', forage$PreyEaten.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Jeong et al 2007b MEPS dinoflagellates'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * forage$X3D.Arena.size[sel] * 10)
forage$PreyAvail.units[sel] <- gsub('per ml', '', forage$PreyAvail.units[sel] )
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel] )
forage$PreyEaten.units[sel] <- gsub('per day', '', forage$PreyEaten.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Galarowicz & Wahl 2005'
sel <- grepl(study, forage$Source) & grepl('per l', forage$PreyAvail.units)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 72)
forage$PreyAvail.units[sel] <- gsub('per l', '', forage$PreyAvail.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Galarowicz & Wahl 2005'
sel <- grepl(study, forage$Source) & grepl('per m2', forage$PreyAvail.units)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] / (2.54/100 * 12 * 2.54/100 * 24))
forage$PreyAvail.units[sel] <- gsub('per m2', '', forage$PreyAvail.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Galarowicz & Wahl 2005'
sel <- grepl(study, forage$Source) & grepl('per m3', forage$PreyAvail.units)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] / 0.072)
forage$PreyAvail.units[sel] <- gsub('per m3', '', forage$PreyAvail.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Johansson 1987'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 90)
forage$PreyAvail.units[sel] <- gsub('per l', '', forage$PreyAvail.units[sel] )
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel] * 10)
forage$PreyEaten.units[sel] <- gsub('per s', '', forage$PreyEaten.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Hansen et al 1990'
sel <- grep(study, forage$Source)
sel <- forage$Data.set %in% 1973:1978
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 2200)
forage$PreyAvail.units[sel] <- gsub('per l', '', forage$PreyAvail.units[sel] )
forage$Trial.duration..h.[sel] <- 1

sel <- forage$Data.set %in% 1883:1895
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 550)
forage$PreyAvail.units[sel] <- gsub('per l', '', forage$PreyAvail.units[sel] )
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel] * 100)
forage$Trial.duration..h.[sel] <- 1

study <- 'Ryer et al 2002'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 93)
forage$PreyAvail.units[sel] <- gsub('per l', '', forage$PreyAvail.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Ryther 1954'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 100)
forage$PreyAvail.units[sel] <- gsub('per ml', '', forage$PreyAvail.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Persson and Greenberg 1990'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 198)
forage$PreyAvail.units[sel] <- gsub('per l', '', forage$PreyAvail.units[sel] )
forage$PreyEaten[sel] <- forage$PreyEaten[sel] * 60
forage$PreyEaten.SE[sel] <- forage$PreyEaten.SE[sel] * 60
forage$PreyEaten.units[sel] <- gsub('per sec', '', forage$PreyEaten.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Persson 1986'
sel <- grep(study, forage$Source)
forage$PreyAvail[sel] <- round(forage$PreyAvail[sel] * 200)
forage$PreyAvail.units[sel] <- gsub('per l', '', forage$PreyAvail.units[sel] )
forage$PreyEaten[sel] <- forage$PreyEaten[sel] * 60
forage$PreyEaten.SE[sel] <- forage$PreyEaten.SE[sel] * 60
forage$PreyEaten.units[sel] <- gsub('per sec', '', forage$PreyEaten.units[sel] )
forage$Trial.duration..h.[sel] <- 1

study <- 'Bergman 1987'
# sel <- grep(study, forage$Source)
sel <- forage$Data.set %in% 1500:1509
forage$PreyAvail[sel] <- forage$PreyAvail[sel] * 90
forage$PreyAvail.units[sel] <- gsub('per l', '', forage$PreyAvail.units[sel] )
forage$PreyEaten[sel] <- round(forage$PreyEaten[sel] * 20, 2)
forage$PreyEaten.SE[sel] <- round(forage$PreyEaten.SE[sel] * 20, 2)
forage$PreyEaten.units[sel] <- gsub('per sec', '', forage$PreyEaten.units[sel] )
forage$Trial.duration..h.[sel] <- 1

# sel <- grep(study, forage$Source)
# sel <- forage$Data.set %in% 1500
# forage[sel,]
# unique(forage[sel,]$Data.set)
# unique(forage[sel,]$Source)
# forage[sel,]$PreyAvail - forage[sel,]$PreyEaten
# plot(forage[sel,]$PreyAvail, forage[sel,]$PreyEaten)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Match-up units of prey available and prey eaten -- TIME ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
forage$PreyAvail.units <- trimws(forage$PreyAvail.units)
forage$PreyEaten.units <- trimws(forage$PreyEaten.units)

# Inspect unit combinations
u <- unique(forage[, c('PreyAvail.units', 'PreyEaten.units', 'Trial.duration..h.')])
# u <- u[ do.call(order, u), ]


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

  if(all(forage$Rate.is[sel] == 'raw data') & 
     all(!is.na(forage$PreyEaten[sel]))){ # These come from NAs for predator abundance
    if(any(!is.wholenumber(forage$PreyEaten[sel]))) {
      forage$PreyEaten[sel] <- my.signif(forage$PreyEaten[sel]) # round to 2 sf
      scale <- 10^maxdecimalplaces(forage$PreyEaten[sel])
      forage$PreyEaten[sel] <- scale * forage$PreyEaten[sel] # rescale to integers
      forage$Trial.duration..h.[sel] <- 1
      
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
# u[ do.call(order, u), ]

u <- unique(forage[, c('PreyAvail.units', 'PreyEaten.units', 'X2D.Arena..units')])
# u[ do.call(order, u), ]

u <- unique(forage[, c('PreyAvail.units', 'PreyEaten.units', 'X3D.Arena..units')])
# u[ do.call(order, u), ]

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
  scale <- 1
  if(forage_meta$Prey.replaced.[sel.meta] == FALSE & 
     all(!is.na(forage$PreyEaten[sel]))){ # These come from NAs for predator abundance
        if(any(forage$PreyEaten[sel] > forage$PreyAvail[sel])){
          scale <- ceiling(max(forage$PreyEaten[sel] / forage$PreyAvail[sel]))
          forage$PreyAvail[sel] <- scale * forage$PreyAvail[sel] 
          forage_meta$PreyAvail.Rescaled[sel.meta] <- TRUE
          forage_meta$PreyAvail.RescalingFactor[sel.meta] <- scale
        }
  }
  if(any(!is.wholenumber(forage$PreyAvail[sel]))) {
    forage$PreyAvail[sel] <- my.signif(forage$PreyAvail[sel]) # round to 2 sf
    scale2 <- 10^maxdecimalplaces(forage$PreyAvail[sel])
    forage$PreyAvail[sel] <- scale2 * forage$PreyAvail[sel] 
    scale <- scale * scale2
    forage_meta$PreyAvail.Rescaled[sel.meta] <- TRUE
    forage_meta$PreyAvail.RescalingFactor[sel.meta] <- scale
  }
  b <- b + 1
  setTxtProgressBar(pb, b)
}
close(pb)

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


# ~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~
# If no Standard Error is given for mean study ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~
# Infer it from the global relationship
sel <- forage$Rate.is=='mean' & 
        is.finite(forage$Sample.size..per.density.) & 
        is.finite(forage$PreyEaten.SE) &
        forage$PreyEaten.SE > 0 &
        forage$PreyEaten > 0
fit <- lm( log(forage$PreyEaten.SE[sel]) ~ 
             log(forage$PreyEaten[sel]) + forage$Sample.size..per.density.[sel] )

sel <- forage$Rate.is=='mean' & 
        is.finite(forage$Sample.size..per.density.) & 
        !is.finite(forage$PreyEaten.SE) &
        is.finite(forage$PreyEaten) & 
        forage$PreyEaten > 0
forage$PreyEaten.SE[sel] <- exp(predict(fit, data.frame(log(forage$PreyEaten[sel]), 
                                 forage$Sample.size..per.density.[sel])))

# Not possible when PreyEaten = 0, so for these infer within-study
sel <- forage$Rate.is=='mean' & 
  is.finite(forage$Sample.size..per.density.) & 
  !is.finite(forage$PreyEaten.SE) &
  is.finite(forage$PreyEaten) & 
  forage$PreyEaten == 0

for (i in unique(forage[sel,]$Data.set)){
  temp.forage <- forage[forage$Data.set==i,]
  tsel <- temp.forage$PreyEaten == 0 & !is.finite(temp.forage$PreyEaten.SE)
  fit <- lm( temp.forage$PreyEaten.SE[!tsel] ~ temp.forage$PreyEaten[!tsel] )
  forage[forage$Data.set==i,]$PreyEaten.SE[tsel] <- max(0, coef(fit)[1])
}


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
    prey.major.grouping = unique(temp.info$Major.grouping.1.1),
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
    study.info$sample.size <- sum(data$n)
  } else{
    data <- data[, -c(4, 5)] # remove 'se' and 'n' from raw data studies
    study.info$sample.size <- nrow(data)
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
