# example 8.8: fully conditional specification multiple imputation for a multilevel regression with random intercepts

library(lme4)
library(mitml)
library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/problemsolving2level.rda"
load(gzcon(url(data_url, open = "rb")))

impute <- rblimp_fcs(
    data = problemsolving2level,
    clusterid = 'school',
    ordinal = 'frlunch',
    fixed = 'probsolve1 condition',
    variables = 'probsolve2 probsolve1 stanmath frlunch teachexp condition ',
    seed = 90291,
    burn = 1000,
    iter = 10000,
    nimps = 100,
    chains = 100)

output(impute)

# mitml list
implist <- as.mitml(impute)

# analysis
model <- "probsolve2 ~ probsolve1  + stanmath + frlunch + teachexp + condition + (1 | school)"
analysis <- with(implist, lmer(model, REML = T))

# pooling + significance tests with barnard & rubin degrees of freedom
df <- 29 - 5 - 1
pooled <- testEstimates(analysis, extra.pars = T, df.com = df)
pooled
confint(pooled, level = .95)
