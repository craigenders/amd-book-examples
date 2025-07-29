# EXAMPLE 8.4 - Model-Based Multiple Imputation for MLM w Cross-Level Interaction

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(lme4)
library(mitml)
library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/employee.rda"
load(gzcon(url(data_url, open = "rb")))

impute <- rblimp(
  data = employee,
  clusterid = 'team',
  fixed = 'male',
  center = '
      groupmean = lmx;  # center at latent group means
      grandmean =  male climate cohesion',  # center at grand means
  model = 'empower ~ lmx male cohesion climate lmx*climate | lmx', # automatic multivariate distribution for incomplete predictors and latent response scores 
  simple = ' lmx | climate', # conditional effect of lmx at different levels of climate 
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 100,
  chains = 100)

output(impute)
simple_plot(empower ~ lmx | climate, impute)

# mitml list
implist <- as.mitml(impute)

# pooled grand means
mean_male <- mean(unlist(lapply(implist, function(data) mean(data$male))))
mean_climate <- mean(unlist(lapply(implist, function(data) mean(data$climate))))
mean_cohesion <- mean(unlist(lapply(implist, function(data) mean(data$cohesion))))

# center at latent cluster means
for (i in 1:length(implist)) {
  implist[[i]]$lmx.cwc <- implist[[i]]$lmx - implist[[i]]$lmx.mean.team.
}

# analysis
model <- "empower ~ lmx.cwc + I(male - mean_male) + I(climate - mean_climate) + I(cohesion - mean_cohesion) + I(lmx.cwc * (climate - mean_climate)) + (1 + lmx.cwc | team)"
analysis <- with(implist, lmer(model, REML = T))

# pooling + significance tests with barnard & rubin degrees of freedom
df <- 105 - 5 - 1
pooled <- testEstimates(analysis, extra.pars = T, df.com = df)
pooled
confint(pooled, level = .95)
