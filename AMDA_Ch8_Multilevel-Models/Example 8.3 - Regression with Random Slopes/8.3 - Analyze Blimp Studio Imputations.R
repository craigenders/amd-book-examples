# EXAMPLE 8.3 - Model-Based Multiple Imputation for MLM w Random Slopes

# remotes::install_github("bkeller2/fdir")

library(fdir)
library(lme4)
library(mitml)

# set working directory to location of this script
set()

# read imputed data from working directory
imps <- read.table("./imps/imps.dat")
names(imps) <- c('Imputation','person','day','pain','sleep','posaff','negaff','lifegoal','female','educ','diagnoses','activity',
                 'painaccept','catastrophize','stress','ranicept.l2','ranslp.l2','pain.mean.person.','sleep.l2mean')

# mitml list
implist <- as.mitml.list(split(imps, imps$Imputation))

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
