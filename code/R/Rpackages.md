
# 1. module unload python/anaconda3-latest
# 2. module unload gcc/5.1.0
# 3. module load R/4.2.1
# 4. module load gcc/9.2.0
# 5. export OS=Linux
# 6. R
# >>> install.packages("xxx")


packages <- 
  c(
   'bbmle',
    'nloptr',
    'lamW',
    'shape',
    'HelpersMG',
    'odeintr'
  )
    
  install.packages(packages, dependencies = TRUE)
  invisible(lapply(packages, library, character.only = TRUE))
