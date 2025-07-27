# example 5.9: bayesian means and correlations

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/employee.rda"
load(gzcon(url(data_url, open = "rb")))

# default prior2
analysis1 <- rblimp(
  data = employee,
  model = 'jobsat empower lmx <-> jobsat empower lmx', 
  seed = 90291,
  burn = 1000,
  iter = 10000)

output(analysis1)

# prior3
analysis2 <- rblimp(
  data = employee,
  model = 'jobsat empower lmx <-> jobsat empower lmx', 
  seed = 90291,
  burn = 1000,
  iter = 10000,
  options = 'prior3')

output(analysis2)

# prior1
analysis3 <- rblimp(
  data = employee,
  model = 'jobsat empower lmx <-> jobsat empower lmx', 
  seed = 90291,
  burn = 1000,
  iter = 10000,
  options = 'prior3')

output(analysis3)

# phantom variable correlation w separation prior
analysis4 <- rblimp(
    data = employee,
    model = 'jobsat empower lmx <-> jobsat empower lmx', # covariances / correlations 
    seed = 90291,
    burn = 10000,
    iter = 20000,
    options = 'use_phantom')  

output(analysis4)

