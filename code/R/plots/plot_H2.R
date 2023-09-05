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
ffr.fits <- profile_coefs(
  ffr.fits,
  model = 'Holling.II',
  point.est = 'median',
  which.pars = c("attack","handling")
)

save(ffr.fits, file = '../../results/fits_profiled/ffr.fits.prof.H2.Rdata')
# load('../../results/fits_profiled/ffr.fits.prof.H2.Rdata')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# General data and plot preparations
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fit.order <-
  order.of.fits(ffr.fits,
                order = TRUE,
                model = "Holling.II",
                order.parm = "attack")
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
    x$estimates[['Holling.II']]["50%", 'attack', "estimate"]))
parm.a <- exp(parm.a)

parm.h <-
  unlist(lapply(ffr.fits, function(x)
    x$estimates[['Holling.II']]["50%", 'handling', "estimate"]))
parm.h <- exp(parm.h)

scaler <- unlist(lapply(ffr.fits, function(x)
  x$study.info$Prey.density.ratio))


par(mfrow=c(1,2))
plot(parm.a, parm.h,
     log = 'xy',
     xlab = 'attack rate',
     ylab = 'handling time (hours)',
     axes = FALSE)
eaxis(1)
eaxis(2)
box(lwd = 1)

plot(parm.a * scaler, parm.h,
     log = 'xy',
     xlab = 'attack rate',
     ylab = 'handling time (hours)',
     axes = FALSE)
eaxis(1)
eaxis(2)
box(lwd = 1)


###################################################
# ~~~~~~~~~~~~~~~~~~ H2 attack ~ ~~~~~~~~~~~~~~~~~~
###################################################
pdf(
  '../../results/H2_a.pdf',
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
  model = "Holling.II",
  parameter = "attack",
  ilink = exp,
  point.est = 'median',
  plot.SEs = TRUE,
  display.outlier.ests = TRUE,
  xlab = "Holling Type II attack rate (a)",
  labels = labels,
  xlim = c(0, 1)
)
dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Alternative / additional summary plots
parm <- parm.a

pdf(
  '../../results/H2_a.pdf',
  height = 3,
  width = 4
)
par(cex = 0.7,
    mgp = c(1.5, 0.1, 0),
    tcl = -0.1)
ylim <- c(1E-6, 1E3)
plot(
  parm ~ sample.sizes,
  ylim = ylim,
  type = 'n',
  log = 'xy',
  xlab = 'Sample size (n)',
  ylab = 'Holling Type II attack rate (a)'
)
arrows(sample.sizes[parm > ylim[2]], 1 * ylim[2], 
       sample.sizes[parm > ylim[2]], 1.03 * ylim[2], 
       length = 0.02)
text(sample.sizes[parm > ylim[2]], 0.98 * ylim[2], 
     round(parm[parm > ylim[2]], 0), cex = 0.5)
points(sample.sizes, parm,
       pch = 21, bg = 'grey')
dev.off()


