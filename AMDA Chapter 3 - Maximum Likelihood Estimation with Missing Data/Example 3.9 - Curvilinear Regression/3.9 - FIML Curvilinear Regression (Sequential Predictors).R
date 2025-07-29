# EXAMPLE 3.9 - FIML Curvilinear Regression (Sequential Predictors)

library(lavaan)
library(mdmb)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/math.rda"
load(gzcon(url(data_url, open = "rb")))

# specify lavaan model for sample statistics
model <- '
  mathpost ~ 1
  anxiety ~ 1
  frlunch ~ 1
  mathpre ~ 1
  male ~ 1
'

# estimate sample statistics in lavaan for centering
means <- inspectSampleCov(model, math, meanstructure = TRUE, missing = "fiml")$mean

# center anxiety at fiml mean
math$anxiety.cgm <- math$anxiety - means["anxiety"]

# summarize incomplete predictors to determine ranges for pseudo-imputations
summary(math[,c("mathpost","anxiety","frlunch","mathpre","male")])

# set ranges (nodes) for pseudo-imputations
nodes.frlunch <- c(0,1)
nodes.anxiety <- seq(-10, 65, by = 1)
nodes.mathpost <- seq(25, 95, by = 1)

# model for frlunch predictor (frlunch | mathpre male)
model.frlunch <- list("model"="logistic", "formula" = frlunch ~ mathpre + male, nodes.frlunch)

# model for anxiety predictor (anxiety | frlunch mathpre male)
model.anxiety <- list( "model"="linreg", "formula" = anxiety.cgm ~ frlunch + mathpre + male, nodes = nodes.anxiety)

# model for mathpost outcome (mathpost | anxiety anxiety^2 frlunch mathpre male
# anxiety mean = 18.058
model.mathpost <- list("model" = "linreg", "formula" = mathpost ~ anxiety.cgm + I(anxiety.cgm^2) + frlunch + mathpre + male, nodes = nodes.mathpost)

# combine predictor models into a list
predictor.models <- list(frlunch = model.frlunch, anxiety = model.anxiety)

# estimate factored regression model w mdmb
fit <- frm_em(dat = math, dep = model.mathpost, ind = predictor.models) 
summary(fit)