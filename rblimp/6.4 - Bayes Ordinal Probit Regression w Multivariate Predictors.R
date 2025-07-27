# example 6.4: bayesian ordinal probit regression with partially factored (multivariate) distribution for incomplete predictors and latent responses

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/employee.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
    data = employee,
    ordinal = 'jobsat male',
    fixed = 'male',
    model = 'jobsat ~ lmx empower male', # automatic multivariate distribution for incomplete predictors and latent response scores 
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(analysis)

