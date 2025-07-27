# example 5.8: bayesian  regression with sequential specification for auxiliary variables

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/drugtrial.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
    data = drugtrial,
    ordinal = 'male drug',
    fixed = 'male drug',
    center = 'severity0 male',
    model = '
      focal.model: severity6 ~ drug severity0 male; # automatic multivariate distribution for incomplete predictors and latent response scores 
    auxiliary.variable.models: severity1 severity3 ~ severity6 drug severity0 male', # automatic sequential specification for variables to the left of the tilde 
    seed = 90291,
    burn = 1000,
    iter = 10000)

output(analysis)
