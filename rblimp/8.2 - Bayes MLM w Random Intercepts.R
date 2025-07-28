# example 8.2: bayesian multilevel regression with random intercepts and partially factored (multivariate) distribution for incomplete predictors and latent response scores

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/problemsolving2level.rda"
load(gzcon(url(data_url, open = "rb")))

# default prior2 (ig prior for level-2 variance with  ss = 0 and df = -2)
analysis1 <- rblimp(
    data = problemsolving2level,
    clusterid = 'school',
    ordinal = 'frlunch',
    fixed = 'probsolve1 condition',
    model = 'probsolve2 ~ probsolve1 stanmath frlunch teachexp condition', # automatic multivariate distribution for incomplete predictors and latent response scores 
    seed = 90291,
    burn = 2000,
    iter = 10000)

output(analysis1)

# prior1 (ig prior for level-2 variance with  ss = 1 and df = 2)
analysis2 <- rblimp(
  data = problemsolving2level,
  clusterid = 'school',
  ordinal = 'frlunch',
  fixed = 'probsolve1 condition',
  model = 'probsolve2 ~ probsolve1 stanmath frlunch teachexp condition', # automatic multivariate distribution for incomplete predictors and latent response scores 
  seed = 90291,
  burn = 2000,
  iter = 10000,
  options = 'prior1')

output(analysis2)

# prior3 (ig prior for level-2 variance with  ss = 0 and df = 0)
analysis3 <- rblimp(
  data = problemsolving2level,
  clusterid = 'school',
  ordinal = 'frlunch',
  fixed = 'probsolve1 condition',
  model = 'probsolve2 ~ probsolve1 stanmath frlunch teachexp condition', # automatic multivariate distribution for incomplete predictors and latent response scores 
  seed = 90291,
  burn = 2000,
  iter = 10000,
  options = 'prior3')

output(analysis3)

