# EXAMPLE 3.2 - FIML Means and Covariances

library(lavaan)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/employee.rda"
load(gzcon(url(data_url, open = "rb")))

# specify model
model <- '
  jobsat ~~ empower
  jobsat ~~ lmx
  empower ~~ lmx
'

# estimate model in lavaan
fit <- sem(model, employee, meanstructure = TRUE, fixed.x = FALSE, missing = "fiml")
summary(fit, standardize = TRUE)

# missing data patterns and proportion observed data (coverage)
inspect(fit, "patterns")
inspect(fit, "coverage")

