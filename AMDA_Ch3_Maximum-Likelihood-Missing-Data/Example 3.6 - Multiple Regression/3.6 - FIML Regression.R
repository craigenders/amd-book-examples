# EXAMPLE 3.6 - FIML Regression

library(lavaan)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/smoking.rda"
load(gzcon(url(data_url, open = "rb")))

# specify model with labels
model <- 'intensity ~ b1*parsmoke + b2*age + b3*income'
wald.constraints <- 'b1 == 0; b2 == 0; b3 == 0'

# estimate model in lavaan
fit <- sem(model, smoking, fixed.x = F, missing = "fiml")
summary(fit, rsquare = T, standardize = T)

# wald test that all slopes equal 0
lavTestWald(fit, constraints = wald.constraints)

# missing data patterns and proportion observed data (coverage)
inspect(fit, "patterns")
inspect(fit, "coverage")
