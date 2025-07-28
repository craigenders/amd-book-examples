# example 7.11: model-based multiple imputation for a moderated regression analysis

library(mitml)
library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/pain.rda"
load(gzcon(url(data_url, open = "rb")))

impute <- rblimp(
    data = pain,
    ordinal = 'male pain',
    fixed = 'male',
    model = '
      focal.model: 
      disability ~ depress male depress*male pain; # automatic multivariate distribution for incomplete predictors and latent response scores 
      auxiliary.variable.models: 
      anxiety stress control interfere ~ disability depress male pain', # automatic sequential specification for auxiliary variables to the left of the tilde 
    simple = 'depress | male',
    seed = 90291,
    burn = 1000,
    iter = 10000,
    nimps = 100, # setting nimps = chains generates one data set from the last iteration of each mcmc chain
    chains = 100)

output(impute)
simple_plot(disability ~ depress | male, impute)

# mitml list
implist <- as.mitml(impute)

# analysis
analysis <- with(implist, lm(disability ~ depress + male + I(depress * male) + pain))

# pooling + significance tests with barnard & rubin degrees of freedom
df <- 275 - 4 - 1
pooled <- testEstimates(analysis, extra.pars = T, df.com = df)
pooled
confint(pooled, level = .95)
