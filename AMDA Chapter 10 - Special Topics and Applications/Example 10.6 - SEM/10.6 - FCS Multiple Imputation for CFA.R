# EXAMPLE 10.6 - FCS Multiple Imputation for CFA

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(lavaan.mi)
library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/eatingrisk.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp_fcs(
    data = eatingrisk,
    ordinal = 'drive1:drive7 diet1:diet5',
    variables = 'bmi drive1:drive7 diet1:diet5',
    seed = 90291,
    burn = 10000,
    iter = 10000,
    nimps = 100,
    chains = 100)

output(impute)

# mitml list
implist <- as.mitml(impute)

# cfa model
lavaan_model <- '
  drive ~~ diet
  drive =~ 1*drive1 + drive2 + drive3 + drive4 + drive5 + drive6 + drive7
  diet =~ 1*diet1 + diet2 + diet3 + diet4 + diet5
'

# cfa model w latent response variable indicators
lavaan_model_latent <- '
  drive ~~ diet
  drive =~ 1*drive1.latent + drive2.latent + drive3.latent + drive4.latent + drive5.latent + drive6.latent + drive7.latent
  diet =~ 1*diet1.latent + diet2.latent + diet3.latent + diet4.latent + diet5.latent
'

# fit model with ml and latent response scores
pooled_ml <- cfa.mi(lavaan_model_latent, data = implist, estimator = "ml")
summary(pooled_ml, standardized = T, fit = T)

# imputation-based modification indices
modindices.mi(pooled_ml, op = c("~~","=~"), minimum.value = 3, sort. = T)

# fit model with wls
pooled_wls <- cfa.mi(lavaan_model, data = implist, std.lv = T, estimator = "wls")
summary(pooled_wls, standardized = T)

# imputation-based modification indices
modindices.mi(pooled_wls, op = c("~~","=~"), minimum.value = 3, sort. = T)
