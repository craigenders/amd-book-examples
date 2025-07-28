# EXAMPLE 8.5 - Model-Based Multiple Imputation for 3-Level MLM

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(lme4)
library(mitml)
library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/problemsolving3level.rda"
load(gzcon(url(data_url, open = "rb")))

impute <- rblimp(
  data = problemsolving3level,
  clusterid = 'school student',
  ordinal = 'frlunch condition',
  fixed = 'month7 condition',
  center = 'grandmean = stanmath frlunch teachexp',
  model = 'probsolve ~ month7 stanmath frlunch teachexp condition month7*condition | month7', # random intercepts and slopes at level-2 and level-3 
  simple = ' month7 | condition', # conditional effect of time within condition 
  seed = 90291,
  burn = 15000,
  iter = 20000,
  nimp = 20,
  chains = 20)

output(impute)
simple_plot(probsolve ~ month7 | condition, impute)

# mitml list
implist <- as.mitml(impute)

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
