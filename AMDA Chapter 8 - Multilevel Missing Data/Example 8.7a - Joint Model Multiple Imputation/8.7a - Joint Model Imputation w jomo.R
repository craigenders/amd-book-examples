# example 8.7: joint model mulilevel multiple imputation w jomo

library(jomo)
library(lme4)
library(mitml)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/problemsolving2level.rda"
load(gzcon(url(data_url, open = "rb")))

# define binary variable as a factor
dat$frlunch <- factor(dat$frlunch, levels = c(0,1))

# select variables for imputation
dat$icept <- 1
vars2impute_l1 <- c("frlunch","stanmath","probsolve2")
vars2impute_l2 <- c("teachexp")
complete_l1 <- c("icept","probsolve1")
complete_l2 <- c("icept","condition")

# joint model imputation with jomo
set.seed(90291)
imps <- jomo(dat[vars2impute_l1], Y2 = dat[vars2impute_l2], X = dat[complete_l1], X2 = dat[complete_l2], 
                   clus = dat$school, nburn = 2000, nbetween = 2000, nimp = 100)

# analysis
implist <- as.mitml.list(split(imps, imps$Imputation))
model <- "probsolve2 ~ probsolve1  + stanmath + frlunch + teachexp + condition + (1 | clus)"
analysis <- with(implist, lmer(model, REML = T))

# pooling + significance tests with barnard & rubin degrees of freedom
df <- 29 - 5 - 1
pooled <- testEstimates(analysis, extra.pars = T, df.com = df)
pooled
confint(pooled, level = .95)


