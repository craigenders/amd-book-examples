# example 6.5: bayesian regression with partially factored (multivariate) distribution for incomplete predictors and latent responses

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/smoking.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
    data = smoking,
    ordinal = 'parsmoke',
    fixed = 'age',
    center = 'income age',
    model = 'intensity ~ parsmoke income age', # automatic multivariate distribution for incomplete predictors and latent response scores 
    seed = 90291,
    burn = 1000,
    iter = 10000)

output(analysis)
