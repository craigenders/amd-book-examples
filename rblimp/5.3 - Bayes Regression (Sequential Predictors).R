# example 5.3: bayesian regression with sequential specification for incomplete predictors

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/employee.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
    data = employee,
    ordinal = 'male',
    fixed = 'male',
    model = '
      focal.model: empower ~ lmx climate male;
      predictor.model: lmx climate ~ male', # automatic sequential specification for variables to the left of the tilde 
    seed = 90291,
    burn = 1000,
    iter = 10000)

output(analysis)

