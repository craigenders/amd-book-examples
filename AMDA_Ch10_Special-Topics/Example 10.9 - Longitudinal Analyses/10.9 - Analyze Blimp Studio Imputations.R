# EXAMPLE 10.9 - FCS Multiple Imputation for SEM Growth Curve

# remotes::install_github("bkeller2/fdir")

library(fdir)
library(lavaan.mi)

# set working directory to location of this script
set()

# read imputed data from working directory
imps <- read.table("./imps/imps.dat")
names(imps) <- c('Imputation','id','male','drug','severity0','severity1','severity3','severity6','dropgrp','earlydrop','latedrop','dropout','sdrop3','sdrop6')

# mitml list
implist <- as.mitml.list(split(imps, imps$Imputation))

# growth model
lavaan_model <- '
  # Latent factors
  icepts =~ 1*severity0 + 1*severity1 + 1*severity3 + 1*severity6
  slopes =~ 0*severity0 + 1*severity1 + 1.732051*severity3 + 2.44949*severity6
  
  # Factor variances and covariance
  icepts ~~ slopes
  
  # Constrain residual variances to be equal across time
  severity0 ~~ resid*severity0
  severity1 ~~ resid*severity1
  severity3 ~~ resid*severity3
  severity6 ~~ resid*severity6
  
  # Factor means
  icepts ~ 1 + drug
  slopes ~ 1 + drug
  
  # Residual means fixed to 0
  severity0 ~ 0*1
  severity1 ~ 0*1
  severity3 ~ 0*1
  severity6 ~ 0*1
'

# fit model with ml and latent response scores
pooled <- cfa.mi(lavaan_model, data = implist, estimator = "ml")
summary(pooled, standardized = T, fit = T)