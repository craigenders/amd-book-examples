# EXAMPLE 2.10 - FIML Regression

library(lavaan)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/smokingcomplete.rda"
load(gzcon(url(data_url, open = "rb")))

# center variables at their grand means
smokingcomplete$age.cgm <- smokingcomplete$age - mean(smokingcomplete$age)
smokingcomplete$income.cgm <- smokingcomplete$income - mean(smokingcomplete$income)

# specify model
model <- 'intensity ~ parsmoke + age.cgm + income.cgm'

# regression model
fit <- sem(model, smokingcomplete, meanstructure = T, fixed.x = T)
summary(fit, rsquare = T, standardize = T)

# bootstrap standard errors and test statistics
fit <- sem(model, smokingcomplete, meanstructure = T, fixed.x = T, se = "bootstrap")
summary(fit, rsquare = T, standardize = T)

# robust standard errors and test statistics
fit <- sem(model, smokingcomplete, meanstructure = T, fixed.x = T, estimator = "MLR")
summary(fit, rsquare = T, standardize = T)

# specify model with labels
model.labels <- 'intensity ~ b1*parsmoke + b2*age.cgm + b3*income.cgm'
wald.constraints <- 'b1 == 0; b2 == 0; b3 == 0'

# wald test that all slopes equal 0
fit <- sem(model.labels, smokingcomplete, meanstructure = T, fixed.x = T, estimator = "MLR")
summary(fit, rsquare = T, standardize = T)
lavTestWald(fit, constraints = wald.constraints)

# specify model with constraints
model.constraints <- 'intensity ~ b1*parsmoke + b2*age.cgm + b3*income.cgm
                      b1 == 0; b2 == 0; b3 == 0'

# constraining slopes to 0 gives LRT versus the saturated model
fit <- sem(model.constraints, smokingcomplete, meanstructure = T, fixed.x = T, estimator = "MLR")
summary(fit, rsquare = T, standardize = T)


