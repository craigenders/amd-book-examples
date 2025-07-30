# EXAMPLE 10.9 - FCS Multiple Imputation for SEM Growth Curve

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(lavaan.mi)
library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/drugtrial.rda"
load(gzcon(url(data_url, open = "rb")))

impute <- rblimp_fcs(
    data = drugtrial,
    fixed = 'drug',
    variables = 'drug severity0 severity1 severity3 severity6',
    seed = 90291,
    burn = 10000,
    iter = 100000,
    chains = 100,
    nimps = 100)

output(impute)

# mitml list
implist <- as.mitml(impute)

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
