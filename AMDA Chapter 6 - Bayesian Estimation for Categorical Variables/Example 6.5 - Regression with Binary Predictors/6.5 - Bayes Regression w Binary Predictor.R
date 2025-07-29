# EXAMPLE 6.5 - Bayes Regression w Binary Predictor

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/smoking.rda"
load(gzcon(url(data_url, open = "rb")))

# multivariate specification for incomplete predictors
analysis1 <- rblimp(
    data = smoking,
    ordinal = 'parsmoke',
    fixed = 'age',
    center = 'income age',
    model = 'intensity ~ parsmoke income age', # automatic multivariate distribution for incomplete predictors and latent response scores 
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(analysis1)

# sequential specification for incomplete predictors
analysis2 <- rblimp(
  data = smoking,
  ordinal = 'parsmoke',
  fixed = 'age',
  center = 'income age',
  model = '
      focal.model: 
      intensity ~ parsmoke income age;
      predictor.models: 
      parsmoke income ~ age', # automatic sequential specification for variables to the left of the tilde 
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(analysis2)
