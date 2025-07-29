# EXAMPLE 8.2 - Model-Based Multiple Imputation for MLM w Random Intercepts

# remotes::install_github("bkeller2/fdir")

library(fdir)
library(lme4)
library(mitml)

# set working directory to location of this script
set()

# read imputed data from working directory
imps <- read.table("./imps/imps.dat")
names(imps) <- c("Imputation","school","student","condition","teachexp","eslpct","ethnic","male","frlunch",
                 "achievegrp","stanmath","efficacy1","efficacy2","probsolve1","probsolve2",
                 "ranicept.l2", "frlunch.latent", "frlunch.l2mean","stanmath.l2mean", "probsolv1.l2mean") 

# analysis
implist <- as.mitml.list(split(imps, imps$Imputation))
model <- "probsolve2 ~ probsolve1  + stanmath + frlunch + teachexp + condition + (1 | school)"
analysis <- with(implist, lmer(model, REML = T))

# pooling + significance tests with barnard & rubin degrees of freedom
df <- 29 - 5 - 1
pooled <- testEstimates(analysis, extra.pars = T, df.com = df)
pooled
confint(pooled, level = .95)