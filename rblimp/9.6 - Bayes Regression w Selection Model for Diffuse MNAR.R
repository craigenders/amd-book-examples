# example 9.6: bayesian selection regression model for a diffuse mnar process

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/drugtrial.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
    data = drugtrial,
    ordinal = 'dropout',
    fixed = 'male drug',
    center = 'male severity0',
    model = '
      focal.model: severity6 ~ drug severity0 male;
      predictor.model: severity0 ~ drug male', # sequential specification for incomplete predictor auxiliary.variable.models: severity3 severity1 ~ severity6 severity0 drug male, # automatic sequential specification for variables to the left of the tilde missingness.model: dropout ~ severity6 drug', 
    seed = 90291,
    burn = 2000,
    iter = 10000)

output(analysis)
