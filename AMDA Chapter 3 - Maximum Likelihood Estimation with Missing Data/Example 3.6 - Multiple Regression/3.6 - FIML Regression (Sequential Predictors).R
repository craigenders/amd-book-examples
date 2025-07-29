# EXAMPLE 3.6 - FIML Regression (Sequential Predictors)

library(mdmb)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/smoking.rda"
load(gzcon(url(data_url, open = "rb")))

# summarize incomplete predictors to determine ranges for pseudo-imputations
summary(smoking[,c("intensity","parsmoke","income","age")])

# set ranges (nodes) for pseudo-imputations
nodes.income <- seq(1, 20, by = 1)
nodes.parsmoke <- c(0,1)
nodes.intensity <- seq(0, 31, by = 1)

# model for parsmoke predictor (parsmoke | age)
model.parsmoke <- list( "model"="logistic", "formula" = parsmoke ~ age, nodes = nodes.parsmoke)

# model for income predictor (income | parsmoke, age)
model.income <- list( "model"="linreg", "formula" = income ~ parsmoke + age, nodes = nodes.income)

# model for intensity outcome (intensity | parsmoke income age)
# age mean = 21.55
# income mean = 8.43
model.intensity <- list("model" = "linreg", "formula" = intensity ~  parsmoke + income + age, nodes = nodes.intensity)

# combine predictor models into a list
predictor.models <- list(parsmoke = model.parsmoke, income = model.income)

# estimate factored regression model w mdmb
fit <- frm_em(dat = smoking, dep = model.intensity, ind = predictor.models) 
summary(fit)
