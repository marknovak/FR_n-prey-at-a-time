source('lib/profile_coefs.R')
source('lib/plot_coefs.R')
source('lib/depletion_check.R')
source('lib/holling_method_one_predator_one_prey.R')
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# read in the dataset-specific fits into a mega container
ffr.fits <- bundle_fits('../../results/fits')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# now profile the fits
ffr.fits <- profile_coefs(
  ffr.fits,
  model = 'Holling.I',
  point.est = 'median',
  printWarnings = TRUE,
  which.pars = "attack"
)

save(ffr.fits, file = '../../results/fits_profiled/ffr.fits.prof.H1.Rdata')
# load('../../results/fits_profiled/ffr.fits.prof.H1.Rdata')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# General data and plot preparations
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fit.order <-
  order.of.fits(ffr.fits,
                order = TRUE,
                model = "Holling.I",
                order.parm = "Sample size")
ffr.fits <- ffr.fits[fit.order]

labels <-
  unlist(lapply(ffr.fits, function(x)
    x$study.info$datasetName))
labels <- gsub('_', ' ', labels)
sample.sizes <-
  unlist(lapply(ffr.fits, function(x)
    x$study.info$sample.size))
labels <- paste0(labels, ' (', sample.sizes, ')')

###################################################
# ~~~~~~~~~~~~~~~~~~ H1 attack ~ ~~~~~~~~~~~~~~~~~~
###################################################
pdf(
  '../../results/H1_a.pdf',
  height = 6,
  width = 5
)
par(
  mar = c(3, 10, 1, 1),
  mgp = c(1.5, 0.1, 0),
  tcl = -0.1,
  las = 1,
  cex = 0.7
)
plot.coefs(
  ffr.fits,
  model = "Holling.I",
  parameter = "attack",
  ilink = exp,
  point.est = 'median',
  plot.SEs = TRUE,
  display.outlier.ests = TRUE,
  xlab = "Holling Type I attack rate (a)",
  labels = labels,
  xlim = c(0, 1)
)
dev.off()

