# example 8.2: bayesian multilevel regression with random intercepts and a sequential specification for incomplete predictors

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
    iter = 10000)

output(analysis)

