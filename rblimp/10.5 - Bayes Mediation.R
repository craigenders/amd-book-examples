# example 10.5: bayesian mediation analysis

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/pain.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
    data = pain,
    ordinal = 'pain',
    fixed = 'male',
    model = '
      mediation.models: 
      interfere ~ pain@apath;
      depress ~ interfere@bpath pain;
      auxiliary.models: 
      anxiety stress control ~ depress interfere pain', # automatic sequential specification for auxiliary variables 
    parameters = 'indirect = apath*bpath',
    seed = 90291,
    burn = 2000,
    iter = 10000)

output(analysis)
