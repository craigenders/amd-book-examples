# EXAMPLE 7.11 - Model-Based Multiple Imputation for Moderated Regression

# remotes::install_github("bkeller2/fdir")

library(fdir)
library(mitml)

# set working directory to location of this script
set()

# read imputed data from working directory
imps <- read.table("./imps/imps.dat")
names(imps) <- c("Imputation", "id", "txgrp", "male", "age", "edugroup", "workhrs", "exercise", "paingrps", 
                "pain", "anxiety", "stress", "control", "depress", "interfere", "disability",
                paste0("dep", seq(1:7)), paste0("int", seq(1:6)), paste0("dis", seq(1:6)))

# analysis and pooling
implist <- as.mitml.list(split(imps, imps$Imputation))
analysis <- with(implist, lm(disability ~ depress + male + I(depress * male) + pain))
nullmodel <- with(implist, lm(disability ~ 1))

# significance tests with barnard & rubin degrees of freedom
df <- 275 - 4 - 1
pooled <- testEstimates(analysis, extra.pars = T, df.com = df)
pooled
confint(pooled, level = .95)

# wald test that all slopes = 0
testModels(analysis, nullmodel, df.com = df, method = "D1")

# likelihood ratio test that all slopes = 0
testModels(analysis, nullmodel, method = "D3")

# test conditional effects (simple slopes)
slope.male <- c("depress + I(depress * male)*1")
testConstraints(analysis, constraints = slope.male, df.com = df)

slope.female <- c("depress + I(depress * male)*0")
testConstraints(analysis, constraints = slope.female, df.com = df)
