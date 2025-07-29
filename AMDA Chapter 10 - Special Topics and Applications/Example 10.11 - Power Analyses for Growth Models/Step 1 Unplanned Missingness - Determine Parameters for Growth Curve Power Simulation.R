# load libraries
library(fdir)

# this script's location is the working directory
fdir::set()

# load simulation functions
source("Solve MLM Parameters Function.R")

# set random number seed
set.seed(90291)

# time scores
time <- c(0,1,2,3,4)
mean.time <- mean(time)
var.time <- var(time) * (length(time) - 1) / length(time)

# list of parameter values and effect sizes
# X = level-1 time variable, W = level-2 binary predictor
power.inputs <- list(
  mean.Y = c(50), # mean of Y
  mean.X = c(mean.time), # X means
  mean.W = c(.50), # W means
  var.Y = c(100), # total variance of Y
  var.X = c(var.time), # total variances of Xs
  var.W = c(.25), # total variances of Ws
  corr.X = c(0,0), # range of within-cluster X correlations
  corr.W = c(0,0), # range of between-cluster W correlations
  corr.X.W = c(0,0), # range of between-cluster X-W correlations
  corr.raneffects = c(.30,.30), # range of random effect correlations
  icc.Y = c(.50), # intraclass correlations of Y
  icc.X = c(0,0), # range of intraclass correlations for level-1 Xs
  R2.X.w = c(.02), # explained variation due to within-cluster fixed effects
  R2.crosslevel.w = c(.06), # interaction explained variation
  R2.ranslopes.w = c(.10), # random slope explained variation
  R2.increment.b = c(0), # incremental between-cluster explained variation
  weights.X.w = c(1), # fixed effect weights for level-1 Xs
  weights.crosslevel.w = c(1), # interaction weights (X1*Ws, X2*Ws)
  weights.ranslopes.w = c(1), # level-1 X weights for random slope variation
  weights.increment.b = c(0,1) # fixed effect weights for level-2 Xbs and Ws
)

# examine population parameters
solve.MLM.parameters(power.inputs)


