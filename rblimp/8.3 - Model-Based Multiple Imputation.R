# example 8.3: bayesian multilevel regression with random slopes and a partially factored (multivariate) distribution for incomplete predictors and latent response scores

library(lme4)
library(mitml)
library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/diary.rda"
load(gzcon(url(data_url, open = "rb")))

impute <- rblimp(
  data = diary,
  clusterid = 'person',
  fixed = 'female',
  center = '
      groupmean = pain;  # center at latent group means
      grandmean = pain.mean sleep painaccept',  # center at grand means
  model = 'posaff ~ pain@b1 sleep pain.mean@b3 painaccept female | pain', # label within- and between-cluster slope coefficients 
  waldtest = 'b1 = b3', # test whether within- and between-cluster regressions differ
  seed = 90291,
  burn = 5000,
  iter = 10000, 
  nimps = 100,
  chains = 100)

output(impute)

# mitml list
implist <- as.mitml(impute)

# pooled grand means
mean_sleep <- mean(unlist(lapply(implist, function(data) mean(data$sleep))))
mean_pain_l2mean <- mean(unlist(lapply(implist, function(data) mean(data$pain.mean.person.))))
mean_painaccept <- mean(unlist(lapply(implist, function(data) mean(data$painaccept))))

# center at latent cluster means
for (i in 1:length(implist)) {
  implist[[i]]$pain.cwc <- implist[[i]]$pain - implist[[i]]$pain.mean.person.
}

# analysis
model <- "posaff ~ pain.cwc + I(sleep - mean_sleep) + I(pain.mean.person. - mean_pain_l2mean) + I(painaccept - mean_painaccept) + female + (1 + pain.cwc | person)"
analysis <- with(implist, lmer(model, REML = T))

# pooling + significance tests with barnard & rubin degrees of freedom
df <- 132 - 5 - 1
pooled <- testEstimates(analysis, extra.pars = T, df.com = df)
pooled
confint(pooled, level = .95)
