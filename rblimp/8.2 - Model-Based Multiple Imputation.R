# example 8.2: model-based multiple imputation for a multilevel regression with random intercepts

library(lme4)
library(mitml)
library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/problemsolving2level.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
  data = problemsolving2level,
  clusterid = 'school',
  ordinal = 'frlunch',
  fixed = 'condition',
  model = '
      focal.model: probsolve2 ~ probsolve1 stanmath frlunch teachexp condition;
      predictor.models: stanmath frlunch probsolve1 teachexp ~ condition', # automatic sequential specification for level-2 variables followed by level-1 variables 
  seed = 90291,
  burn = 4000,
  iter = 10000,
  nimps = 100,
  chains = 100)

output(analysis)

# mitml list
implist <- as.mitml(analysis)

# analysis and pooling
model <- "probsolve2 ~ probsolve1  + stanmath + frlunch + teachexp + condition + (1 | school)"
pooled <- with(implist, lmer(model, REML = T))

# significance tests with barnard & rubin degrees of freedom
df <- 29 - 5 - 1
estimates <- testEstimates(pooled, extra.pars = T, df.com = df)
estimates
confint(estimates, level = .95)
