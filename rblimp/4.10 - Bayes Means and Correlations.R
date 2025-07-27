# example 4.10: bayesian correlation matrix

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/employeecomplete.rda"
load(gzcon(url(data_url, open = "rb")))

# default prior2
analysis1 <- rblimp(
  data = employeecomplete,
  model = 'jobsat empower lmx <-> jobsat empower lmx', 
  seed = 90291,
  burn = 1000,
  iter = 10000)

output(analysis1)

# prior1
analysis2 <- rblimp(
  data = employeecomplete,
  model = 'jobsat empower lmx <-> jobsat empower lmx', 
  seed = 90291,
  burn = 1000,
  iter = 10000,
  options = 'prior1')

output(analysis2)

# prior3
analysis3 <- rblimp(
  data = employeecomplete,
  model = 'jobsat empower lmx <-> jobsat empower lmx', 
  seed = 90291,
  burn = 1000,
  iter = 10000,
  options = 'prior3')

output(analysis3)

# phantom variable correlation w separation prior
analysis4 <- rblimp(
    data = employeecomplete,
    model = 'jobsat empower lmx <-> jobsat empower lmx', # covariances / correlations 
    seed = 90291,
    burn = 10000,
    iter = 20000,
    options = 'use_phantom')  

output(analysis4)

