# example 8.7: random joint model mulilevel multiple imputation w jomo

library(jomo)
library(lme4)
library(mitml)
library(rockchalk)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/diary.rda"
load(gzcon(url(data_url, open = "rb")))

# define binary variable as a factor
diary$female <- factor(diary$female, levels = c(0,1))

# define level-1 and level-2 variables
diary$icept <- 1
vars2impute_l1 <- c("posaff","pain","sleep")
vars2impute_l2 <- c("painaccept")
complete_l2 <- c("icept","female")

# joint model imputation with random covariance matrices in jomo
set.seed(90291)
imps <- jomo(diary[vars2impute_l1], Y2 = diary[vars2impute_l2], X2 = diary[complete_l2], clus = diary$person, 
             nburn = 2000, nbetween = 2000, nimp = 100, meth = "random")

# mitml list
implist <- as.mitml.list(split(imps, imps$Imputation))
implist <- implist[-1]
class(implist) <- c("mitml.list", "list")

# center at cluster means
for (i in seq_along(implist)) {
  implist[[i]] <- gmc(implist[[i]], c("pain"), by = c("clus"), FUN = mean, suffix = c(".mean.person", ".cwc"), fulldataframe = TRUE)
}

# pooled grand means
mean_sleep <- mean(unlist(lapply(implist, function(data) mean(data$sleep))))
mean_pain_l2mean <- mean(unlist(lapply(implist, function(data) mean(data$pain.mean.person))))
mean_painaccept <- mean(unlist(lapply(implist, function(data) mean(data$painaccept))))

# center at grand means means
for (i in 1:length(implist)) {
  implist[[i]]$sleep.cgm <- implist[[i]]$sleep - mean_sleep
  implist[[i]]$pain.mean.person.cgm <- implist[[i]]$pain.mean.person - mean_pain_l2mean
  implist[[i]]$painaccept.cgm <- implist[[i]]$painaccept - mean_painaccept
}

# analysis
model <- "posaff ~ pain.cwc + sleep.cgm + pain.mean.person.cgm + painaccept.cgm + female + (1 + pain.cwc | clus)"
analysis <- with(implist, lmer(model, REML = T))

# pooling + significance tests with barnard & rubin degrees of freedom
df <- 132 - 5 - 1
pooled <- testEstimates(analysis, extra.pars = T, df.com = df)
pooled
confint(pooled, level = .95)

