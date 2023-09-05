library('sfsmisc')

source('lib/profile_coefs.R')
source('lib/plot_coefs.R')
source('lib/depletion_check.R')
source('lib/holling_method_one_predator_one_prey.R')
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# read in the dataset-specific fits into a mega container
ffr.fits <- bundle_fits('../../results/fits')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# now profile the fits
# ffr.fits <- profile_coefs(
#   ffr.fits,
#   model = 'Holling.n',
#   point.est = 'median',
#   which.pars = c("n")
# )

# save(ffr.fits, file = '../../results/fits_profiled/ffr.fits.prof.Hn.Rdata')
# load('../../results/fits_profiled/ffr.fits.prof.Hn.Rdata')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# General data and plot preparations
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fit.order <-
  order.of.fits(ffr.fits,
                order = TRUE,
                model = "Holling.n",
                order.parm = "n")
ffr.fits <- ffr.fits[fit.order]

labels <-
  unlist(lapply(ffr.fits, function(x)
    x$study.info$datasetName))
labels <- gsub('_', ' ', labels)

sample.sizes <-
  unlist(lapply(ffr.fits, function(x)
    x$study.info$sample.size))

labels <- paste0(labels, ' (', sample.sizes, ')')


parm.a <-
  unlist(lapply(ffr.fits, function(x)
    x$estimates[['Holling.n']]["50%", 'attack', "estimate"]))
parm.a <- exp(parm.a)

parm.h <-
  unlist(lapply(ffr.fits, function(x)
    x$estimates[['Holling.n']]["50%", 'handling', "estimate"]))
parm.h <- exp(parm.h)

parm.n <-
  unlist(lapply(ffr.fits, function(x)
    x$estimates[['Holling.n']]["50%", 'n', "estimate"]))
parm.n <- exp(parm.n)



sub.n <- parm.n
  

transf <- function(x){log2(x)}
rng <- range(transf(sub.n))
brks <- seq(floor(rng[1]), ceiling(rng[2]), length.out = 101)
xat <- seq(floor(rng[1]), ceiling(rng[2]), length.out = 10)
h <- hist(transf(sub.n), breaks = brks, axes = FALSE, col = 'lightblue')
axis(2)
axis(1, at = xat, labels = signif(2^xat, 4))
abline(v = transf(1), lwd = 2, col = 'blue')
abline(v = mean(transf(sub.n)), lwd = 3, col = 'red')
abline(v = median(transf(sub.n)), lwd = 2, col = 'green')
