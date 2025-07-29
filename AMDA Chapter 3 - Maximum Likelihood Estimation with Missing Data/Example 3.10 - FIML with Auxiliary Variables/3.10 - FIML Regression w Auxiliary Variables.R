# EXAMPLE 3.10 - FIML Regression w Auxiliary Variables

library(lavaan)
library(mdmb)
library(semTools)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/drugtrial.rda"
load(gzcon(url(data_url, open = "rb")))

# center at the means
drugtrial$severity0.cgm <- drugtrial$severity0 - mean(drugtrial$severity0, na.rm = T)
drugtrial$male.cgm <- drugtrial$male - mean(drugtrial$male, na.rm = T)

##################################################################
# model without auxiliary variables in lavaan
##################################################################

# specify lavaan model with no auxiliary variables
model <- 'severity6 ~ drug + severity0.cgm + male.cgm'

# estimate model with no auxiliary variables
fit.noaux <- sem(model, drugtrial, fixed.x = F, missing = "fiml")
summary(fit.noaux, rsquare = T, standardize = T)

##################################################################
# saturated correlates model in lavaan
##################################################################

# estimate model with semtools and lavaan
fit.satcor <- sem.auxiliary(model, drugtrial, aux = c("severity1","severity3"))
summary(fit.satcor, rsquare = T, standardize = T)

##################################################################
# extra dependent variable model in lavaan
##################################################################

# specify lavaan model with auxiliary variables as extra outcomes
model.extradv <- 'severity6 ~ drug + severity0.cgm + male.cgm
                  severity1 ~ drug + severity0.cgm + male.cgm
                  severity3 ~ drug + severity0.cgm + male.cgm
                  severity6 ~~ severity1
                  severity6 ~~ severity3
                  severity1 ~~ severity3'

# estimate model in lavaan
fit.extradv <- sem(model.extradv, drugtrial, fixed.x = F, missing = "fiml")
summary(fit.extradv, rsquare = T, standardize = T)

##################################################################
# sequential specification in lavaan
##################################################################

# specify lavaan model with factored (sequential) specification for auxiliary variables
model.seqaux <- 'severity6 ~ drug + severity0.cgm + male.cgm
                 severity3 ~ severity6 + drug + severity0.cgm + male.cgm
                 severity1 ~ severity3 + severity6 + drug + severity0.cgm + male.cgm'

# estimate model in lavaan
fit.seqaux <- sem(model.seqaux, drugtrial, fixed.x = F, missing = "fiml")
summary(fit.seqaux, rsquare = T, standardize = T)

##################################################################
# sequential specification in mdmb
##################################################################

# estimate sample statistics in lavaan for centering
inspectSampleCov(fit.satcor, drugtrial, fixed.x = F, missing = "fiml")

# summarize incomplete variables to determine ranges for pseudo-imputations
summary(drugtrial[,c("severity6","severity0","severity3","severity1")])

# set ranges (nodes) for pseudo-imputations
nodes.severity0 <- seq(1, 7, by = .25)
nodes.severity6 <- seq(1, 7, by = .25)
nodes.severity3 <- seq(1, 7, by = .25)
nodes.severity1 <- seq(1, 7, by = .25)

# model for severity0 predictor (severity1 | drug male)
model.severity0 <- list( "model"="linreg", "formula" = severity0 ~ drug + male, nodes = nodes.severity0)

# model for severity6 outcome (severity6 | severity0 drug male)
# severity0 mean = 5.367
# male mean = .474
model.severity6 <- list("model" = "linreg", "formula" = severity6 ~ drug + I(severity0 - 5.367) + I(male - .474), nodes = nodes.severity6)

# model for severity3 auxiliary variable (severity3 | severity6 severity0 drug male)
model.severity3 <- list( "model"="linreg", "formula" = severity3 ~ severity6 + drug + severity0 + male, nodes = nodes.severity3)

# model for severity1 auxiliary variable (severity1 | severity3 severity6 severity0 drug male)
model.severity1 <- list( "model"="linreg", "formula" = severity1 ~ severity3 + severity6 + drug + severity0 + male, nodes = nodes.severity1)

# combine predictor models into a list (focal model functions as a predictor model)
predictor.models <- list(severity0 = model.severity0, severity6 = model.severity6, severity3 = model.severity3)

# estimate factored regression model w mdmb
fit <- frm_em(dat = drugtrial, dep = model.severity1, ind = predictor.models) 
summary(fit)
