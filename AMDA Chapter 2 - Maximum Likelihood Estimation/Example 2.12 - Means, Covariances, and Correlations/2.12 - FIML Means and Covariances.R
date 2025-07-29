# EXAMPLE 2.12 - FIML Means and Covariances

library(lavaan)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/employeecomplete.rda"
load(gzcon(url(data_url, open = "rb")))

# specify model
model <- '
  jobsat ~~ empower
  jobsat ~~ lmx
  empower ~~ lmx
'

# estimate model in lavaan
fit <- sem(model, employeecomplete, meanstructure = T, fixed.x = F)
summary(fit, standardize = T)
