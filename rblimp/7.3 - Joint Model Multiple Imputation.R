# example 7.3: joint model multiple imputation

library(mitml)
library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/math.rda"
load(gzcon(url(data_url, open = "rb")))

impute <- rblimp(
    data = math,
    ordinal = 'frlunch',
    model = 'mathpost mathpre frlunch stanread ~~ mathpost mathpre frlunch stanread', 
    seed = 90291,
    burn = 1000,
    iter = 1000,
    nimps = 100,  # setting nimps = chains generates one data set from the last iteration of each mcmc chain
    chains = 100)

output(impute)

# compute change score in each data set
impute@imputations <- lapply(impute@imputations, function(data) {
  data$change <- data$mathpost - data$mathpre
  data
})

# mitml list
implist <- as.mitml(impute)

# analysis
analysis <- with(implist, lm(change ~ 1))

# pooling + significance tests with barnard & rubin degrees of freedom
estimates <- testEstimates(analysis, extra.pars = T, df.com = 249)
estimates
confint(estimates, level = .95)


