# example 6.5: bayesian regression with sequential specification for incomplete predictors

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/smoking.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
    data = smoking,
    ordinal = 'parsmoke',
    fixed = 'age',
    center = 'income age',
    model = '
      focal.model: intensity ~ parsmoke income age;
      predictor.models: parsmoke income ~ age', # automatic sequential specification for variables to the left of the tilde 
    seed = 90291,
    burn = 1000,
    iter = 10000)

output(analysis)
