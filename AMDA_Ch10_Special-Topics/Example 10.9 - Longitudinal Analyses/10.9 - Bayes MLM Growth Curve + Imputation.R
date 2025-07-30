# EXAMPLE 10.9 - Bayes MLM Growth Curve + Imputation

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(lme4)
library(mitml)
library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/drugtrial2level.rda"
load(gzcon(url(data_url, open = "rb")))

# mcmc analysis
analysis <- rblimp(
    data = drugtrial2level,
    clusterid = 'id',
    ordinal = 'drug',
    transform = 'sqrtweek = sqrt(week)',
    fixed = 'drug sqrtweek',
    model = 'severity ~ sqrtweek drug sqrtweek*drug | sqrtweek',
    simple = 'sqrtweek | drug', 
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(analysis)
simple_plot(severity ~ sqrtweek | drug, analysis)

# model-based multiple imputation
impute <- rblimp(
  data = drugtrial2level,
  clusterid = 'id',
  ordinal = 'drug',
  transform = 'sqrtweek = sqrt(week)',
  fixed = 'drug sqrtweek',
  model = 'severity ~ sqrtweek drug sqrtweek*drug | sqrtweek',
  simple = ' sqrtweek | drug', 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 100,
  chains = 100)

output(impute)

# mitml list
implist <- as.mitml(impute)

# analysis
model <- "severity ~ sqrtweek + drug + I(sqrtweek * drug) + (1 + sqrtweek | id)"
analysis <- with(implist, lmer(model, REML = T))

# pooling + significance tests with barnard & rubin degrees of freedom
df <- 437 - 3 - 1
pooled <- testEstimates(analysis, extra.pars = T, df.com = df)
pooled
confint(pooled, level = .95)
