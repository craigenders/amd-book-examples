# example 5.7: bayesian curvilinear regression with partially factored (multivariate) distribution for incomplete predictors and latent responses

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/math.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
    data = math,
    ordinal = 'frlunch male',
    fixed = 'male mathpre',
    center = 'anxiety',
    model = 'mathpost ~ anxiety (anxiety^2) frlunch mathpre male', # automatic multivariate distribution for incomplete predictors and latent response scores
    seed = 12345,
    burn = 1000,
    iter = 10000)

output(analysis)

