# example 4.10: bayesian correlation matrix

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/employeecomplete.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
    data = employeecomplete,
    model = 'jobsat empower lmx <-> jobsat empower lmx', # covariances / correlations 
    seed = 90291,
    burn = 10000,
    iter = 20000,
    options = 'use_phantom')  # phantom variable correlation w separation prior

output(analysis)

