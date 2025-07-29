# EXAMPLE 3.11 - FIML Logistic and Probit Regression (Sequential Predictors)

library(mdmb)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/employee.rda"
load(gzcon(url(data_url, open = "rb")))

# summarize incomplete variables to determine ranges for pseudo-imputations
summary(employee[,c("turnover","lmx","empower","male")])

# set ranges (nodes) for pseudo-imputations
nodes.empower <- seq(0, 50, by = 1)
nodes.lmx <- seq(-5, 32, by = 1)
nodes.turnover <- c(0,1)

# model for empower predictor (empower | male)
model.empower <- list( "model"="linreg", "formula" = empower ~ male, nodes = nodes.empower)

# model for lmx predictor (lmx | empower male)
model.lmx <- list( "model"="linreg", "formula" = lmx ~ empower + male, nodes = nodes.lmx)

# logistic model for turnover outcome (turnover | lmx empower male)
logistic.model.turnover <- list( "model"="logistic", "formula" = turnover ~ lmx + empower + male, nodes = nodes.turnover)

# probit model for turnover outcome (turnover | lmx empower male)
probit.model.turnover <- list( "model"="oprobit", "formula" = turnover ~ lmx + empower + male, nodes = nodes.turnover)

# combine predictor models into a list
predictor.models <- list(empower = model.empower, lmx = model.lmx)

# estimate factored logistic regression model w mdmb
fit.logistic <- frm_em(dat = employee, dep = logistic.model.turnover, ind = predictor.models) 
summary(fit.logistic)

# estimate factored probit regression model w mdmb
fit.probit <- mdmb::frm_em(dat = employee, dep = probit.model.turnover, ind = predictor.models) 
summary(fit.probit)
