# EXAMPLE 8.4 - Model-Based Multiple Imputation for MLM w Cross-Level Interaction

# remotes::install_github("bkeller2/fdir")

library(fdir)
library(lme4)
library(mitml)

# set working directory to location of this script
set()

# read imputed data from working directory
imps <- read.table("./imps/imps.dat")
names(imps) <- c("Imputation", "employee","team","turnover","male","empower","lmx","jobsat","climate","cohesion",
                "ranicept.l2", "ranslp.l2", "lmx.mean.team.")

# analysis and pooling
implist <- as.mitml.list(split(imps, imps$Imputation))

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
