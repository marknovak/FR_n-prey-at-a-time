source('lib/plot_coefs.R') # for plot_coefs() and order.of.fits()
source('lib/plot_obsVfit.R')
source('lib/holling_method_one_predator_one_prey.R')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# read in the dataset-specific fits into a mega container
ffr.fits <- bundle_fits('../../results/fits')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# fit.order <-
#   order.of.fits(ffr.fits,
#                 order = TRUE,
#                 model = "Holling.I",
#                 order.parm = "Sample size")
# ffr.fits <- ffr.fits[rev(fit.order)]

length(ffr.fits)

models <- c(
  'Holling.I',
  'Holling.II',
  'Holling.n'
)

pdf(
  file = "../../results/obsVfit.pdf",
  height = 4.5,
  width = 6,
  onefile = T
)
par(
  mar = c(5, 3, 4, 0.5),
  mgp = c(1.5, 0.1, 0),
  tcl = -0.1,
  las = 1,
  cex = 0.4
)
par(mfcol = c(2, length(models)))
for (i in 1:length(ffr.fits)) {
  titles <- models
  dataset <- ffr.fits[[i]]$study.info$datasetName
  titles[2] <- paste(dataset, '\n', titles[2])
  for (m in 1:length(models)) {
    
    plot_obsVfit(ffr.fits[[i]], models[m], title = titles[m])
    plot_obsVfit(ffr.fits[[i]], models[m], title = '', curve = TRUE)
    
    mtext(
      bquote(LL ==
               .(round(
                 ffr.fits[[i]]$LL[[models[m]]]['mean'], 1 ))
             ~ "(" * .(round(
               ffr.fits[[i]]$LL[[models[m]]]['16%'], 1 ))
             * "," ~ .(round(
               ffr.fits[[i]]$LL[[models[m]]]['84%'], 1 )) * ")"),
      side = 3,
      line = 1,
      cex = 0.5
    )
    mtext(
      bquote(BIC ==
               .(round(
                 ffr.fits[[i]]$BIC[[models[m]]]['mean'], 1 ))
             ~ "(" * .(round(
               ffr.fits[[i]]$BIC[[models[m]]]['16%'], 1 ))
             * "," ~ .(round(
               ffr.fits[[i]]$BIC[[models[m]]]['84%'], 1 )) * ")"),
      side = 3,
      line = 2,
      cex = 0.5
    )
    mtext(
      bquote(AICc ==
               .(round(
                 ffr.fits[[i]]$AICc[[models[m]]]['mean'], 1 ))
             ~ "(" * .(round(
               ffr.fits[[i]]$AICc[[models[m]]]['16%'], 1 ))
             * "," ~ .(round(
               ffr.fits[[i]]$AICc[[models[m]]]['84%'], 1 )) * ")"),
      side = 3,
      line = 3,
      cex = 0.5
    )
    try(mtext(
      bquote(AIC ==
               .(round(
                 ffr.fits[[i]]$AIC[[models[m]]]['mean'], 1 ))
             ~ "(" * .(round(
               ffr.fits[[i]]$AIC[[models[m]]]['16%'], 1 ))
             * "," ~ .(round(
               ffr.fits[[i]]$AIC[[models[m]]]['84%'], 1 )) * ")"),
      side = 3,
      line = 4,
      cex = 0.5
    ))
  }
}
dev.off()
