# EXAMPLE 8.5 - Model-Based Multiple Imputation for 3-Level MLM

# remotes::install_github("bkeller2/fdir")

library(fdir)
library(lme4)
library(mitml)

# set working directory to location of this script
set()

# read imputed data from working directory
imps <- read.table("./imps/imps.dat")
names(imps) <- c("Imputation","school","student","wave","condition","teachexp","eslpct","race",
                 "male","frlunch","achievegrp","stanmath","month0", "month7", "probsolve", "efficacy",
                 "ranicept.l2","ranslp.l2","ranicept.l3","ranslp.l3","frlunch.l2mean","frlunch.l3mean",
                 "stanmath.l3mean","residual.l1")

# analysis and pooling
implist <- as.mitml.list(split(imps, imps$Imputation))

# pooled grand means
mean_stanmath <- mean(unlist(lapply(implist, function(data) mean(data$stanmath))))
mean_frlunch <- mean(unlist(lapply(implist, function(data) mean(data$frlunch))))
mean_teachexp <- mean(unlist(lapply(implist, function(data) mean(data$teachexp))))

# analysis
model <- "probsolve ~ month7  + I(stanmath - mean_stanmath) + I(frlunch - mean_frlunch) + I(teachexp - mean_teachexp) + condition + I(month7 * condition) + (1 + month7 | school/student)"
analysis <- with(implist, lmer(model, REML = T))

# pooling + significance tests with barnard & rubin degrees of freedom
df <- 29 - 6 - 1
pooled <- testEstimates(analysis, extra.pars = T, df.com = df)
pooled
confint(pooled, level = .95)

# test conditional effects
growth.control <- c("month7 + I(month7 * condition)*0")
testConstraints(analysis, constraints = growth.control, df.com = df)
growth.expgrp <- c("month7 + I(month7 * condition)*1")
testConstraints(analysis, constraints = growth.expgrp, df.com = df)