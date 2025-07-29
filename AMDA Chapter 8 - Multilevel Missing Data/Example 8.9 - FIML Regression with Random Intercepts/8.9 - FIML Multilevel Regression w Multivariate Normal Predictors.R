# example 8.9: maximum likelihood estimation for a multilevel regression with random intercepts;

library(lavaan)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/problemsolving2level.rda"
load(gzcon(url(data_url, open = "rb")))

# specify model
lavaan_model <- '
    level: 1
    probsolve2 ~ probsolve1  + stanmath + frlunch
    level: 2
    probsolve2 ~ teachexp + condition
'

# estimate model in lavaan
fit <- sem(lavaan_model, problemsolving2level, cluster = "school", meanstructure = TRUE, fixed.x = FALSE, missing = "fiml")
summary(fit, standardize = TRUE)

# missing data patterns and proportion observed data (coverage)
inspect(fit, "patterns")
inspect(fit, "coverage")