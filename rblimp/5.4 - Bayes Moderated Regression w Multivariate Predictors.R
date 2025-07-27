# example 5.4: bayesian moderated regression with partially factored (multivariate) distribution for incomplete predictors and latent responses

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/pain.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
    data = pain,
    ordinal = 'male pain',
    fixed = 'male',
    center = 'depress',
    model = 'disability ~ depress male depress*male pain', # automatic multivariate distribution for incomplete predictors and latent response scores 
    simple = 'depress | male', 
    seed = 90291,
    burn = 1000,
    iter = 10000)

output(analysis)
simple_plot(disability ~ depress | male, analysis)
