
library('RColorBrewer')
library('VennDiagram')
library('png') # for re-inserting venn diagram plots
library('sfsmisc') # for eaxis
library('scales') # for alpha transparency
library('stargazer') # for LaTeX tables

source('lib/profile_coefs.R')
source('lib/plot_coefs.R')
source('lib/holling_method_one_predator_one_prey.R')
source('lib/addImg.R')

figdir <- '../../figs/'
tabledir <- '../../tables/'

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in the dataset-specific fits into a mega container ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ffr.fits <- bundle_fits('../../results/fits')

# ~~~~~~~~~~~~~~~~
# Profile the fits
# ~~~~~~~~~~~~~~~~
point.est <-
  'median'  # mean or median (median will always be used if SE are plotted)

ffr.fits <- profile_coefs(
  ffr.fits,
  model = 'Holling.n',
  point.est = point.est,
  which.pars = c('attack', 'handling', 'n')
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Note that we do n + 1 given how the model was constrained, so
# Define some convenience functions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
exp.p1 <- function(x) {
  exp(x) + 1
}
inv.exp.p1 <- function(x) {
  log(x - 1)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# General data and plot preparations ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fit.order <-
  order.of.fits(
    ffr.fits,
    order = TRUE,
    model = "Holling.n",
    order.parm = "n",
    point.est = point.est
  )
ffr.fits <- ffr.fits[fit.order]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Repeat following separately for replacement and non-replacement studies ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
replacement <-
  unlist(lapply(ffr.fits, function(x)
    x$study.info$replacement))

ffr.fits.orig <- ffr.fits

for (repl in c('Repl', 'nonRepl', 'All')){

  if(repl == 'Repl'){ 
    ffr.fits <- ffr.fits.orig[which(replacement)] 
    xlims <- c(0.9, 150)
  }
  if(repl == 'nonRepl'){
    ffr.fits <- ffr.fits.orig[which(!replacement)]
    xlims <- c(0.9, 300)
  }
  if(repl == 'All'){
    ffr.fits <- ffr.fits.orig
    xlims <- c(0.9, 300)
  }
  
  labels <-
    unlist(lapply(ffr.fits, function(x)
      x$study.info$datasetName))
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assess frequency of "best" model performance  ----
  # using summary of IC estimates across bootstrapped fits
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  IC <- 'BIC'
  
  ICs <- data.frame(t(sapply(ffr.fits, function(x) {
    unlist(data.frame(
      Holling.I  = x[[IC]][['Holling.I']]['mean'],
      Holling.II = x[[IC]][['Holling.II']]['mean'],
      Holling.n  = x[[IC]][['Holling.n']]['mean'],
      Holling.III  = x[[IC]][['Holling.III']]['mean'],
      Holling.nIII  = x[[IC]][['Holling.nIII']]['mean']
    ))})))
  rownames(ICs) <- labels
  
  
  # Delta-IC and ranks
  minICs <- apply(ICs, 1, min)
  delta.IC <- ICs - minICs
  rnks.IC <- t(apply(ICs, 1, rank, ties.method = 'first'))
  colnames(rnks.IC) <- colnames(ICs)
  
  
  # Define delta IC cut-off for "indistinguishably well performing" models
  cutoff.IC <- 2
  evidence.IC <- data.frame(delta.IC < cutoff.IC)
  
  Hn.single.best.IC <- evidence.IC[, 'Holling.n'] & 
                          !evidence.IC[, 'Holling.II'] &
                            !evidence.IC[, 'Holling.I'] &
                            !evidence.IC[, 'Holling.III'] &
                            !evidence.IC[, 'Holling.nIII'] 
  
  # ~~~~~~~~~~~~
  # Venn diagram ----
  # ~~~~~~~~~~~~
  # Count of freq by which model(s) is "best"
  # Venn diagram gets exported by function, then re-imported below
  cat.names <- c("Type I " , "Type II" , "n-prey", "Type III", "Type nIII")
  venn.diagram(
    x = lapply(as.list(evidence.IC), function(x){(1:nrow(evidence.IC))[x]}),
    category.names = cat.names,
    filename = paste0(figdir, 'Hn_venn_', repl, '.png'),
    output = FALSE,
    
    # Output features
    imagetype="png" ,
    height = 800 , 
    width = 800 , 
    resolution = 400,
    disable.logging = TRUE,
    margin = 0.02,
    
    # Circles
    lwd = 0.1,
    lty = 1,
    fill = brewer.pal(length(cat.names), "Pastel2"),
  
    # Numbers
    cex = 0.6,
    fontface = "bold",
    fontfamily = "sans",
    
    # Set names
    cat.cex = 0.6,
    cat.fontface = "bold",
    cat.default.pos = "outer",
    # cat.pos = c(0, 0, 180),
    cat.fontfamily = "sans"
  )
  venn <- readPNG(paste0(figdir, 'Hn_venn_', repl, '.png'))
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Plot estimates of 'n' along with Venn diagram ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pch.vector <- ifelse(Hn.single.best.IC, 21, 21)
  color.vector <- ifelse(Hn.single.best.IC, 'black', 'black')
  bg.vector <- ifelse(Hn.single.best.IC, 'grey50', 'grey50')
  
  pch.vector <- ifelse(replacement, 21, 21)
  color.vector <- ifelse(replacement, 'grey50', 'grey70')
  bg.vector <- ifelse(replacement, 'grey70', 'grey80')
  
pdf(paste0(figdir, 'Hn_n-', repl,'.pdf'), height = 6, width = 4)
  par(
    mar = c(3, 1, 1, 1),
    mgp = c(1.5, 0.1, 0),
    tcl = -0.1,
    las = 1,
    cex = 0.7
  )
    plot.coefs(
      ffr.fits,
      model = "Holling.n",
      parameter = "n",
      ilink = exp.p1,
      plot.SEs = TRUE,
      display.outlier.ests = TRUE,
      xlab = expression(paste("prey at a time ", ( italic( n ) ))),
      # labels = labels,
      xlim = xlims,
      color.vector = color.vector,
      pch.vector = pch.vector,
      bg.vector = bg.vector,
      log.axis.base = 2
    )
  
    # Add Venn diagram
    par(fig = c(0.55, 1, 0, 0.4), 
        xpd = TRUE,
        new = TRUE) 
    plot(1:10,1:10, 
         type = 'n', 
         axes = FALSE, 
         ann = FALSE)
    addImg(venn, 5, 5, 10)
  
  dev.off()

} # <-- end of loop through replacement vs. non-replacement studies

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pull together estimates and covariates ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ffr.fits <- ffr.fits.orig
labels <- unlist(lapply(ffr.fits, function(x){ x$study.info$datasetName }))

# ~~~~~~~~~~~~~~~~~~~~~~
# Extract the parameters ----
# ~~~~~~~~~~~~~~~~~~~~~~
point.est <- ifelse(point.est == 'median', '50%', point.est)

ests.n <- data.frame(t(sapply(ffr.fits, function(x) {
  unlist(data.frame(
    parm.a  = exp(x$estimates[['Holling.n']][point.est, 'attack', "estimate"]),
    parm.h =  exp(x$estimates[['Holling.n']][point.est, 'handling', "estimate"]),
    # ====> Note that we do n + 1 given how the model was constrained <=====
    parm.n  = exp.p1(x$estimates[['Holling.n']][point.est, 'n', "estimate"]),
    parm.n.lb  = exp.p1(x$profile$cf['n', 'lb']),
    parm.n.ub  = exp.p1(x$profile$cf['n', 'ub'])
  ))})))
rownames(ests.n) <- labels

ests.III <- data.frame(t(sapply(ffr.fits, function(x) {
  unlist(data.frame(
    parm.III.a  = exp(x$estimates[['Holling.III']][point.est, 'attack', "estimate"]),
    parm.III.h =  exp(x$estimates[['Holling.III']][point.est, 'handling', "estimate"]),
    # ====> Note that we do n + 1 given how the model was constrained <=====
    parm.III.m  = exp.p1(x$estimates[['Holling.III']][point.est, 'm', "estimate"])

  ))})))
rownames(ests.III) <- labels

ests.nIII <- data.frame(t(sapply(ffr.fits, function(x) {
  unlist(data.frame(
    parm.nIII.a  = exp(x$estimates[['Holling.nIII']][point.est, 'attack', "estimate"]),
    parm.nIII.h =  exp(x$estimates[['Holling.nIII']][point.est, 'handling', "estimate"]),
    # ====> Note that we do n + 1 given how the model was constrained <=====
    parm.nIII.n  = exp.p1(x$estimates[['Holling.nIII']][point.est, 'n', "estimate"]),
    parm.nIII.m  = exp.p1(x$estimates[['Holling.nIII']][point.est, 'm', "estimate"])
  ))})))
rownames(ests.nIII) <- labels

# Extract covariates by type (to preserve formatting) ----
covars1 <- data.frame(t(sapply(ffr.fits, function(x) {
  unlist(data.frame(
    sample.size = x$study.info$sample.size,
    pred.mass = x$study.info$predator.mass,
    prey.mass = x$study.info$prey.mass,
    pred.prey.mass.ratio = x$study.info$predator.mass / x$study.info$prey.mass,
    dimension = x$study.info$dimension,
    scaling.factor.eaten = x$study.info$rescaled.eaten.scaling.factor,
    scaling.factor.prey = x$study.info$rescaled.prey.scaling.factor
  ))})))

covars2 <- data.frame(t(sapply(ffr.fits, function(x) {
  unlist(data.frame(
    pred.major.group = x$study.info$pred.major.grouping,
    prey.major.group = x$study.info$prey.major.grouping,
    predator.parasite = x$study.info$predator.parasite,
    pred.vert.invert = x$study.info$pred.vert.invert,
    pred.cellularity = x$study.info$predator.cellularity,
    prey.cellularity = x$study.info$prey.cellularity,
    habitat = x$study.info$habitat,
    fresh.marine = x$study.info$fresh.marine
  ))})))

covars3 <- data.frame(t(sapply(ffr.fits, function(x) {
  unlist(data.frame(
    mass.study = x$study.info$mass.study,
    replacement = x$study.info$replacement
  ))})))

covars <- data.frame(covars1, covars2, covars3)
rownames(covars) <- labels

# Equivalent of "half saturation point" (of Holling II) ----
# (asymptotic inflection point for Holling.n)
covars$parm.beta <- round(1 / (ests.n$parm.a * ests.n$parm.h), 1)

# Largest prey abundance observed/employed
covars$max.Nprey <-  unlist(lapply(ffr.fits, function(x){
                              max(x$study.info$data.Nprey)} ))


dat <- data.frame(ests.n, ests.III, ests.nIII, covars)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Venn diagrams by predator group ----
# ~~~~~~~~~~~~
pmg <- table(dat$pred.major.group)
pmg <- pmg[order(pmg, decreasing = TRUE)]
cumsum(pmg)/sum(pmg)

focal.preds <- names(pmg[which(pmg > 100)])
focal.preds.cols <- brewer.pal(length(focal.preds), 'Accent')

for (p in 1:length(focal.preds)){
temp.evidence.IC <- evidence.IC[covars$pred.major.group == focal.preds[p],]
  venn.diagram(
    x = lapply(as.list(temp.evidence.IC), function(x){(1:nrow(temp.evidence.IC))[x]}),
    category.names = cat.names,
    filename = paste0(figdir, 'Hn_venn_', focal.preds[p], '.png'),
    output = FALSE,
    
    # Output features
    imagetype="png" ,
    height = 800 , 
    width = 800 , 
    resolution = 400,
    disable.logging = TRUE,
    margin = 0.02,
    
    # Circles
    lwd = 0.1,
    lty = 1,
    fill = brewer.pal(length(cat.names), "Pastel2"),
    
    # Numbers
    cex = 0.6,
    fontface = "bold",
    fontfamily = "sans",
    
    # Set names
    cat.cex = 0.6,
    cat.fontface = "bold",
    cat.default.pos = "outer",
    # cat.pos = c(0, 0, 180),
    cat.fontfamily = "sans"
  )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Analyses of 'n' estimates ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Remove studies that were deemed to be Type I only
# (We can use evidence.IC since 'All' is the last subset run above)
T1 <- evidence.IC$Holling.I & apply(evidence.IC, 1, sum) == 1
# evidence.IC[T1,]

dat <- dat[!T1,]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# cumulative distribution of n estimates ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cumdens <- ecdf(log2(dat$parm.n))

pdf(paste0(figdir, 'Hn_n-ecdf.pdf'), height = 3, width = 4.25)
par(
  mar = c(3, 3, 1, 1),
  mgp = c(1.5, 0.2, 0),
  tcl = -0.1,
  las = 1,
  cex = 0.7,
  yaxs = 'i')

  plot(cumdens,
       col.01line = NA,
       axes = FALSE,
       main = '',
       xlab = expression(paste('Prey at a time ', (italic(n)))),
       ylab = expression(paste('Probability ', P(italic(X) <= italic(n)))),
       ylim = c(0.5, 1.05),
       verticals = TRUE,
       do.points = FALSE
  )
  xat <- 0:10
  axis(1, at = xat, labels = 2^xat)
  axis(2)
  box(lwd = 1)
  xat <- c(2, 4, 8, 16, 32)
  segments(log2(xat), 0, log2(xat), cumdens(log2(xat)),
           col = 'grey80',
           lty = 2)
  segments(-10, cumdens(log2(xat)), log2(xat), cumdens(log2(xat)),
           col = 'grey80',
           lty = 2)
  legend(
    'bottomright', 
    ncol = 2L,
    inset = 0.01,
    legend = c(expression(italic(n)), 
               xat, 
               expression(P(italic(X) <= italic(n))),
               round(cumdens(log2(xat)), 3))
  )
dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# n vs. pred-prey body-mass ratio ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

base.ppmr <- 10
base.n <- 2
dat$log.ppmr <- log(dat$pred.prey.mass.ratio, base.ppmr)
dat$log.n <- log(dat$parm.n, base.n)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Subset to datasets having both pred & prey body mass values ----

sel <- is.finite(dat$pred.prey.mass.ratio)

pdf(paste0(figdir, 'Hn_n-ppmr.pdf'), height = 2.5, width = 3.25)
par(
  mar = c(3, 3, 1, 1),
  mgp = c(1.5, 0.2, 0),
  tcl = -0.1,
  las = 1,
  cex = 0.7
)
  plot(parm.n ~ pred.prey.mass.ratio, 
       data = dat[sel,],
       log = 'xy',
       pch = 21,
       bg = 'grey',
       xlab = 'Predator-prey body-mass ratio',
       ylab = expression('Prey at a time ' (italic(n))),
       axes = FALSE)
  eaxis(1, at = base.ppmr^seq(-2,14,2))
  eaxis(2, at = base.n^seq(-2, 10), 
        labels = base.n^seq(-2, 10), 
        use.expr = FALSE)

  fit <- lm(log.n ~ log.ppmr, 
            data = dat[sel,])
  summary(fit)
  fit.all <- fit
  
  xrng <- range(dat$log.ppmr[sel])
  dts <- data.frame(log.ppmr = seq(xrng[1], xrng[2], length = 100) )
  dts$pred <- predict(fit, newdata = dts)
  lines(base.ppmr^(dts$log.ppmr), 
        base.n^(dts$pred), 
        col = 'black',
        lwd = 3)
  lines(base.ppmr^(dts$log.ppmr), 
        base.n^(dts$pred), 
        col = 'blue',
        lwd = 2)

  
  # Subset further to estimates of n > 1 ----
  sel <- is.finite(dat$pred.prey.mass.ratio) & round(dat$parm.n) > 1

  fit <- lm(log.n ~ log.ppmr, 
            data = dat[sel,])
  summary(fit)
  fit.ng1 <- fit
  
  xrng <- range(dat$log.ppmr[sel])
  dts <- data.frame(log.ppmr = seq(xrng[1], xrng[2], length = 100) )
  dts$pred <- predict(fit, newdata = dts)
  lines(base.ppmr^(dts$log.ppmr), 
        base.n^(dts$pred), 
        col = 'black',
        lwd = 3)
  lines(base.ppmr^(dts$log.ppmr), 
        base.n^(dts$pred), 
        col = 'red',
        lwd = 2)
  
  legend('topright',
         legend = c(expression(n > 1), expression(n >= 1)),
         lty = 1,
         lwd = 3,
         text.col = 'white',
         col = 'black',
         inset = 0,
         bty = 'n'
  )  
  legend('topright',
         legend = c(expression(n > 1), expression(n >= 1)),
         lty = 1,
         lwd = 2,
         col = c('red', 'blue'),
         inset = 0,
         bty = 'n'
  )
  box(lwd = 1)
dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# n vs. body size by predator identity ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sel <- is.finite(dat$pred.prey.mass.ratio) &
        dat$pred.major.group %in% focal.preds

fit <- lm(log.n ~
            log.ppmr * factor(pred.major.group, levels = focal.preds),
          data = dat[sel,])
summary(fit)
fit.foc.preds <- fit


pdf(paste0(figdir, 'Hn_n-ppmr_byPred.pdf'), height = 2.5, width = 3.25)
par(
  mar = c(3, 3, 1, 1),
  mgp = c(1.5, 0.2, 0),
  tcl = -0.1,
  las = 1,
  cex = 0.7
)
  sel <- is.finite(dat$pred.prey.mass.ratio)
  plot(parm.n ~ pred.prey.mass.ratio, 
       data = dat[sel,],
       log = 'xy',
       pch = 21,
       col = alpha('grey10', 0.3),
       bg = alpha('grey', 0.3),
       xlab = 'Predator-prey body-mass ratio',
       ylab = expression('Prey at a time ' (italic(n))),
       axes = FALSE)
  eaxis(1, at = base.ppmr^seq(-2,14,2))
  eaxis(2, at = base.n^seq(-2, 10), 
        labels = base.n^seq(-2, 10), 
        use.expr = FALSE)
  
  for (p in 1:length(focal.preds)){
    focal.pred <-   focal.preds[p]
    sel <- is.finite(dat$pred.prey.mass.ratio) &
              dat$pred.major.group %in% focal.pred
    points(dat$pred.prey.mass.ratio[sel],
           dat$parm.n[sel],
           pch = 21,
           bg = focal.preds.cols[p])
  }
  for (p in 1:length(focal.preds)){
    focal.pred <-   focal.preds[p]
    sel <- is.finite(dat$pred.prey.mass.ratio) &
      dat$pred.major.group %in% focal.pred
    xrng <- range(dat$log.ppmr[sel])
    dts <- data.frame(log.ppmr = seq(xrng[1], xrng[2], length = 100) )
    dts$pred.major.group <- focal.pred
    dts$pred <- predict(fit, newdata = dts)
    lines(base.ppmr^(dts$log.ppmr), 
          base.n^(dts$pred), 
          col = 'black',
          lwd = 3)
    lines(base.ppmr^(dts$log.ppmr), 
          base.n^(dts$pred), 
          col = focal.preds.cols[p],
          lwd = 2)
  }
    legend('topright',
           legend = c(focal.preds, 'Other'),
           lty = c(rep(1, length(focal.preds)), NA),
           lwd = c(rep(3, length(focal.preds)), NA),
           pch = c(rep(NA, length(focal.preds)), 21),
           pt.bg = c(rep(NA, length(focal.preds)), alpha('grey', 0.3)),
           col = c(rep('black', length(focal.preds)), alpha('grey10', 0.3)),
           text.col = 'white',
           inset = 0,
           bty = 'n'
    )
    legend('topright',
           legend = c(focal.preds, 'Other'),
           lty = c(rep(1, length(focal.preds)), NA),
           lwd = c(rep(2, length(focal.preds)), NA),
           pch = c(rep(NA, length(focal.preds)), 21),
           pt.bg = c(rep(NA, length(focal.preds)), alpha('grey', 0.3)),
           col = c(focal.preds.cols, alpha('grey10', 0.3)),
           inset = 0,
           bty = 'n'
    )
    box(lwd = 1)
dev.off()
         
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# n vs. body size (high sample size studies only) ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf(paste0(figdir, 'Hn_n-ppmr_SSgMed.pdf'), height = 2.5, width = 3.25)
par(
  mar = c(3, 3, 1, 1),
  mgp = c(1.5, 0.2, 0),
  tcl = -0.1,
  las = 1,
  cex = 0.7
)
  sel <- is.finite(dat$pred.prey.mass.ratio)
  plot(parm.n ~ pred.prey.mass.ratio, 
       data = dat[sel,],
       log = 'xy',
       pch = 21,
       col = alpha('grey10', 0.2),
       bg = alpha('grey', 0.2),
       xlab = 'Predator-prey body-mass ratio',
       ylab = expression('Prey at a time ' (italic(n))),
       axes = FALSE)
  eaxis(1, at = base.ppmr^seq(-2,14,2))
  eaxis(2, at = base.n^seq(-2, 10), 
        labels = base.n^seq(-2, 10), 
        use.expr = FALSE)
  
  
  med <- median(dat$sample.size)
  sel <- is.finite(dat$pred.prey.mass.ratio) &
      dat$sample.size > med
  points(dat$pred.prey.mass.ratio[sel],
           dat$parm.n[sel],
           pch = 21,
           bg = 'grey')
  
  fit <- lm(log.n ~ log.ppmr, 
            data = dat[sel,])
  fit.SSMed <- fit
  
    xrng <- range(dat$log.ppmr[sel])
    dts <- data.frame(log.ppmr = seq(xrng[1], xrng[2], length = 100) )
    dts$pred.major.group <- focal.pred
    dts$pred <- predict(fit, newdata = dts)
    lines(base.ppmr^(dts$log.ppmr), 
          base.n^(dts$pred), 
          col = 'black',
          lwd = 3)
    lines(base.ppmr^(dts$log.ppmr), 
          base.n^(dts$pred), 
          col = 'grey',
          lwd = 2)
  
  box(lwd = 1)
dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~
# Export summary tables ----
# ~~~~~~~~~~~~~~~~~~~~~~~~
covarlab <- paste0('$log_{', base.ppmr, '}$(PPMR)')

stargazer(fit.all, fit.ng1,
          column.labels = c('$n \\geq$ 1','$n$ \\textgreater 1'),
          dep.var.caption = 'Estimates',
          dep.var.labels = '',#'Prey at-a-time (n)',
          dep.var.labels.include = FALSE,
          intercept.bottom = FALSE,
          model.numbers = FALSE,
          covariate.labels = c('Intercept',
                               covarlab),
          ci = TRUE, ci.level = 0.95, 
          single.row = TRUE,
          align = FALSE,
          notes.label = '',
          label = 'tab:n-ppmr',
          title = paste0("Summary statistics (with 95\\% confidence intervals) for the least-squares linear regressions of $log_2(n)$ of the multi-prey model on ", covarlab, " when considering all studies ($n \\geq$ 1) or only those studies for which $n$ \\textgreater 1."),
          # float.env = "sidewaystable",
          out = paste0(tabledir,'Hn_n-ppmr.tex')
          )

stargazer(fit.foc.preds,
          dep.var.caption = 'Focal predators',
          dep.var.labels = '',#'Prey at-a-time (n)',
          dep.var.labels.include = FALSE,
          intercept.bottom = FALSE,
          covariate.labels = c(paste0('Intercept (', focal.preds[1], ')'),
                                covarlab,
                                focal.preds[-1],
                                paste0(covarlab,':',focal.preds[-1])),
          ci = TRUE, ci.level = 0.95, 
          single.row = TRUE,
          align = FALSE,
          notes.label = '',
          label = 'tab:n-ppmr_byPred',
          title = paste0("Summary statistics (with 95\\% confidence intervals) for the multiple least-squares linear regression of $log_2(n)$ of the multi-prey model on ", covarlab, " $\\times$ predator group for the four most common predator taxonomic groups."),
          # float.env = "sidewaystable",
          out = paste0(tabledir,'Hn_n-ppmr_byPred.tex')
)

stargazer(fit.SSMed,
          dep.var.caption = paste0('Sample size \\textgreater ', med),
          dep.var.labels = '',#'Prey at-a-time (n)',
          dep.var.labels.include = FALSE,
          intercept.bottom = FALSE,
          covariate.labels = c('Intercept',
                               covarlab),
          ci = TRUE, ci.level = 0.95, 
          single.row = TRUE,
          align = FALSE,
          notes.label = '',
          label = 'tab:n-ppmr_ssgMed',
          title = paste0("Summary statistics (with 95\\% confidence intervals) for the least-squares linear regression of $log_2(n)$ of the multi-prey model on ", covarlab, " when considering only those studies having a sample size greater than the median sample size of all studies."),
          # float.env = "sidewaystable",
          out = paste0(tabledir,'Hn_n-ppmr_SSgMed.tex')
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~
# Additional investigations of n ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~

# Predator-prey mass-ratios by replacement vs. non-replacement studies ----
stats <- function(x){c(
  'mean' = mean(na.omit(x)), 
  'sd'=sd(na.omit(x)))
}

ppmr <- aggregate(log10(dat$pred.prey.mass.ratio), 
          by = list(replacement = dat$replacement), 
          FUN = stats)
ppmr[,-1] <- 10^ppmr[,-1]
ppmr

breaks = 45
hist(log10(dat$pred.prey.mass.ratio), 
     breaks = breaks,
     col = 'grey40')
hist(log10(dat$pred.prey.mass.ratio[dat$replacement==FALSE]), 
     breaks = breaks,
     col = 'grey80',
     add = TRUE)

#~~~~~~~~~~~~~~~~~

sel <- is.finite(dat$pred.prey.mass.ratio)
fit <- lm(log.n ~ 
            log.ppmr + 
            predator.parasite + 
            pred.vert.invert +
            replacement +
            dimension + 
            habitat +
            prey.cellularity
          , 
          data = dat[sel,])
summary(fit)

fit <- lm(log.n ~ 
            log.ppmr + 
            replacement +
            dimension + 
            habitat +
            pred.major.group +
            prey.major.group
          , 
          data = dat[sel,])
summary(fit)


#~~~~~~~~~~~~~~~~~~~~~~~~~~
# Hill exponent of Type III and nIII ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~
dat$log.ppmr <- log(dat$pred.prey.mass.ratio, base.ppmr)
dat$log.m <- log(dat$parm.III.m, base.n)

sel <- is.finite(dat$pred.prey.mass.ratio)

pdf(paste0(figdir, 'HIII_m-ppmr.pdf'), height = 2.5, width = 3.25)
par(
  mar = c(3, 3, 1, 1),
  mgp = c(1.5, 0.2, 0),
  tcl = -0.1,
  las = 1,
  cex = 0.7
)
  plot(parm.III.m ~ pred.prey.mass.ratio, 
       data = dat[sel,],
       log = 'xy',
       pch = 21,
       bg = 'grey',
       xlab = 'Predator-prey body-mass ratio',
       ylab = expression('Hill exponent ' (italic(phi))),
       axes = FALSE)
  eaxis(1, at = base.ppmr^seq(-2,14,2))
  eaxis(2, at = base.n^seq(-2, 10), 
        labels = base.n^seq(-2, 10), 
        use.expr = FALSE)

  fit <- lm(log.m ~ log.ppmr, 
            data = dat[sel,])
  summary(fit)
  fit.m.all <- fit
  
  xrng <- range(dat$log.ppmr[sel])
  dts <- data.frame(log.ppmr = seq(xrng[1], xrng[2], length = 100) )
  dts$pred <- predict(fit, newdata = dts)
  lines(base.ppmr^(dts$log.ppmr), 
        base.n^(dts$pred), 
        col = 'black',
        lwd = 3)
  lines(base.ppmr^(dts$log.ppmr), 
        base.n^(dts$pred), 
        col = 'blue',
        lwd = 2)

  
  # Subset further to estimates of n > 1 ----
  sel <- is.finite(dat$pred.prey.mass.ratio) & round(dat$parm.III.m) > 1

  fit <- lm(log.m ~ log.ppmr, 
            data = dat[sel,])
  summary(fit)
  fit.m.g1 <- fit
  
  xrng <- range(dat$log.ppmr[sel])
  dts <- data.frame(log.ppmr = seq(xrng[1], xrng[2], length = 100) )
  dts$pred <- predict(fit, newdata = dts)
  lines(base.ppmr^(dts$log.ppmr), 
        base.n^(dts$pred), 
        col = 'black',
        lwd = 3)
  lines(base.ppmr^(dts$log.ppmr), 
        base.n^(dts$pred), 
        col = 'red',
        lwd = 2)
  
  legend('topright',
         legend = c(expression(phi > 1), expression(phi >= 1)),
         lty = 1,
         lwd = 3,
         text.col = 'white',
         col = 'black',
         inset = 0,
         bty = 'n'
  )  
  legend('topright',
         legend = c(expression(phi > 1), expression(phi >= 1)),
         lty = 1,
         lwd = 2,
         col = c('red', 'blue'),
         inset = 0,
         bty = 'n'
  )
  box(lwd = 1)
dev.off()


stargazer(fit.m.all, fit.m.g1,
          column.labels = c('$\\phi \\geq$ 1','$\\phi$ \\textgreater 1'),
          dep.var.caption = 'Estimates',
          dep.var.labels = '',
          dep.var.labels.include = FALSE,
          intercept.bottom = FALSE,
          model.numbers = FALSE,
          covariate.labels = c('Intercept',
                               covarlab),
          ci = TRUE, ci.level = 0.95, 
          single.row = TRUE,
          align = FALSE,
          notes.label = '',
          label = 'tab:n-ppmr',
          title = paste0("Summary statistics (with 95\\% confidence intervals) for the least-squares linear regressions of $log_2(\\phi)$ of the Holling-Real Type III on ", covarlab, " when considering all studies ($\\phi \\geq$ 1) or only those studies for which $\\phi$ \\textgreater 1."),
          # float.env = "sidewaystable",
          out = paste0(tabledir,'HIII_m-ppmr.tex')
          )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Relationship between n and Hill exponent (m) ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Across n-prey and Type III
# and 
# within generalized Type nIII


fit <- lm(log.m~log.n,
          data = dat)

pdf(paste0(figdir, 'nVm.pdf'), height = 2.5, width = 6.5)
par(
  mar = c(3, 3, 1, 1),
  mgp = c(1.5, 0.2, 0),
  tcl = -0.1,
  las = 1,
  cex = 0.6,
  mfrow = c(1, 2)
)
  plot(parm.III.m~parm.n ,
       data = dat,
       log = 'xy',
       pch = 21,
       bg = 'grey',
       cex = 0.8,
       xlab = expression('Prey at a time ' (italic(n))),
       ylab = expression('Hill exponent ' (italic(phi))),
       axes = FALSE)
  abline(0, 1, 
         lty = 2,
         col = 'grey')
  abline(fit,
         lty = 1,
         col = 'grey20')
  eaxis(1, at = base.n^seq(-2, 10), 
        labels = base.n^seq(-2, 10), 
        use.expr = FALSE)
  eaxis(2, at = base.n^seq(-2, 10), 
        labels = base.n^seq(-2, 10), 
        use.expr = FALSE)
  mtext('(A)', side = 3, line = 0, at = 1)
  
  plot(parm.nIII.m ~ parm.nIII.n, 
       data = dat,
       log = 'xy',
       pch = 21,
       bg = 'grey',
       cex = 0.8,
       xlab = expression('Prey at a time ' (italic(n))),
       ylab = expression('Hill exponent ' (italic(phi))),
       axes = FALSE)
    abline(0, 1, 
         lty = 2,
         col = 'grey')
    eaxis(1, at = base.n^seq(-2, 10), 
        labels = base.n^seq(-2, 10), 
        use.expr = FALSE)
    eaxis(2, at = base.n^seq(-2, 10), 
        labels = base.n^seq(-2, 10), 
        use.expr = FALSE)
      mtext('(B)', side = 3, line = 0, at = 1)
dev.off()





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~