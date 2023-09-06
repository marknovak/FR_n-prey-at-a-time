mytidy <- function(fit) {
  if (typeof(fit) == 'S4') {
    out <- coef(summary(fit))
    colnames(out) <-
      c('estimate', 'std.error', 'statistic', 'p.value')
    return(out)
  } else{
    return(NA)
  }
}

make.array <- function(ffr.fit, boot.reps) {
  if (typeof(ffr.fit) == 'S4') {
    ffr.fit <- mytidy(ffr.fit)
  }
  out <- array(NA, dim = c(dim(ffr.fit), boot.reps))
  dimnames(out) <- dimnames(ffr.fit)
  return(out)
}

summarize.boots <- function(x) {
  c(
    mean = mean(x, na.rm = TRUE),
    quantile(x, c(0.025, 0.16, 0.5, 0.84, 0.975), na.rm = TRUE),
    n = sum(!is.na(x))
  )
}
