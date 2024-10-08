
# 1. module unload python/anaconda3-latest
# 2. module unload gcc/5.1.0
# 3. module load R/4.2.1
# 4. module load gcc/9.2.0
# 5. export OS=Linux
# 6. R
# >>> install.packages("xxx")


# CMake is needed (or nloptr (on which other used packages also depend), thus 
# before installing the R package run "module load cmake/3.30.3" at the command line.

packages <- 
  c(
   'bbmle',
    'nloptr',
    'lamW',
    'shape',
    'HelpersMG',
    'progress',
    'stargazer',
    'devtools'
  )
    
  install.packages(packages, dependencies = TRUE)
  invisible(lapply(packages, library, character.only = TRUE))

library(devtools)
install_github('thk686/odeintr')
library(odeintr)