# EXAMPLE 3.8 - FIML Moderated Regression (Sequential Predictors)

library(lavaan)
library(mdmb)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/pain.rda"
load(gzcon(url(data_url, open = "rb")))

# specify lavaan model for sample statistics
model <- '
  disability ~ 1
  depress ~ 1
  male ~ 1
  pain ~ 1
'

# estimate sample statistics in lavaan for centering
means <- inspectSampleCov(model, pain, meanstructure = TRUE, missing = "fiml")$mean

# center anxiety at fiml mean
pain$depress.cgm <- pain$depress - means["depress"]

# summarize incomplete predictors to determine ranges for pseudo-imputations
summary(pain[,c("disability","depress.cgm","male","pain")])

# set ranges (nodes) for pseudo-imputations
nodes.pain <- c(0,1)
nodes.depress <- seq(-10, 15, by = 1) 
nodes.disability <- seq(0, 45, by = 1)

# model for pain predictor (pain | male)
model.pain <- list( "model"="logistic", "formula" = pain ~ male, nodes = nodes.pain)

# model for depression predictor (depress | pain male)
model.depress <- list( "model"="linreg", "formula" = depress.cgm ~ pain + male, nodes = nodes.depress)

# model for disability outcome (disability | depress male depress*male pain)
# depression mean = 14.718
model.disability <- list("model" = "linreg", "formula" = disability ~ depress.cgm + male + I(depress.cgm * male) + pain, nodes = nodes.disability)

# combine predictor models into a list
predictor.models <- list(pain = model.pain, depress = model.depress)

# estimate factored regression model w mdmb
fit <- frm_em(dat = pain, dep = model.disability, ind = predictor.models) 
summary(fit)