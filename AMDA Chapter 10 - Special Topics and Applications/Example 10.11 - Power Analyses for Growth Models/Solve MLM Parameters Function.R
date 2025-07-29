solve.MLM.parameters <- function(power.inputs){
    
options(scipen = 999)

#####################################################################################
# specify the level-1 covariate x's mean, variance, and ICC
#####################################################################################

# setting the grand mean to 0 simplifies the distribution of the product
mean.X <- power.inputs$mean.X
# specify arbitrary within-cluster variances of Xs
var.X <- power.inputs$var.X
# specify X's intraclass correlation
icc.X <- power.inputs$icc.X
# correlations among Xs are the same at level-1 and level-2
corr.X <- power.inputs$corr.X

#####################################################################################
# specify the level-2 covariate W's mean, variance, and level-2 correlation with X
#####################################################################################

# setting the grand mean to 0 simplifies solutions 
mean.W <- rep(0, length(power.inputs$mean.W))
# specify arbitrary between-cluster variances of Ws
var.W <- power.inputs$var.W
# between-cluster correlation of Ws and X group means
corr.W <- power.inputs$corr.W
corr.X.W <- power.inputs$corr.X.W

#####################################################################################
# specify the dependent variable's parameters
#####################################################################################

# setting the grand mean to an arbitrary value
mean.Y <- power.inputs$mean.Y
# specifying a larger value for the Y variance produces larger slope coefficients (avoids rounding imprecision)
var.Y <- power.inputs$var.Y
# specify y's intraclass correlation
icc.Y <- power.inputs$icc.Y

#####################################################################################
# specify the correlation between the random intercept and slope residuals
#####################################################################################

corr.raneffects <- power.inputs$corr.raneffects

#####################################################################################
# specify r-square statistics
#####################################################################################

# specify proportion of within-cluster variance explained by the fixed effect of Xs
# relative contributions of within-cluster Xs
R2.X.w <- power.inputs$R2.X.w
weights.X.w <- power.inputs$weights.X.w

# specify proportion of within-cluster variance explained by the fixed effect of XW
# X and XW are orthogonal, so these sum to R2.fixed.w
R2.crosslevel.w <- power.inputs$R2.crosslevel.w
weights.crosslevel.w <- power.inputs$weights.crosslevel.w

# specify proportion of within-cluster variance explained by random slopes
R2.ranslopes.w <- power.inputs$R2.ranslopes.w
weights.ranslopes.w <- power.inputs$weights.ranslopes.w

# option 1: specify incremental proportion of between-cluster variance explained by the fixed effect of Ws
# the slopes of all X group means are fixed at the within-cluster coefficient (i.e., no contextual effect)
# the weights of all X group means are NA
# option 2: specify the proportion of between-cluster explained variation and relative weights for all between-cluster predictors 
# predictor set and weights include X group means and Ws
R2.increment.b <- power.inputs$R2.increment.b
weights.increment.b <- power.inputs$weights.increment.b
weights.increment.b[weights.increment.b=="covariate"] <- NA
weights.increment.b <- as.numeric(weights.increment.b)

#####################################################################################
# solve model parameters: do not alter code below
#####################################################################################

# variable counts
num.X <- length(mean.X)
num.W <- length(mean.W)

# compute within and between-cluster variances
icc.X <- runif(num.X,icc.X[1],icc.X[2])
icc.X[icc.X == 0] <- .0000000000001
icc.X[weights.increment.b[1:num.X] == 0] <- .0000000000001
var.X.b <- var.X * icc.X
var.X.w <- var.X - var.X.b
var.Y.b <- var.Y * icc.Y
var.Y.w <- var.Y - var.Y.b

# construct the within-cluster covariance matrix of the Xs
num.corrs <- num.X * (num.X - 1) / 2
cor.XX.w <- matrix(1, nrow = num.X, ncol = num.X)
cor.XX.w[lower.tri(cor.XX.w, diag = F)] <- cor.XX.w[upper.tri(cor.XX.w, diag = F)] <- runif(num.corrs,corr.X[1],corr.X[2])
if(length(cor.XX.w) == 1){phi.XX.w <- cor.XX.w * sqrt(var.X.w * var.X.w)} else{
  phi.XX.w <- diag(sqrt(var.X.w)) %*% cor.XX.w %*% diag(sqrt(var.X.w))
}

# construct the between-cluster covariance matrix of the X group means
# X correlations are equal at level-1 and level-2
if(length(cor.XX.w) == 1){phi.XX.b <- cor.XX.w * sqrt(var.X.b * var.X.b)} else{
  phi.XX.b <- diag(sqrt(var.X.b)) %*% cor.XX.w %*% diag(sqrt(var.X.b))
}

# construct the between-cluster covariance matrix of the Ws
num.corrs <- num.W * (num.W - 1) / 2
cor.WW.b <- matrix(1, nrow = num.W, ncol = num.W)
cor.WW.b[lower.tri(cor.WW.b, diag = F)] <- cor.WW.b[upper.tri(cor.WW.b, diag = F)] <- runif(num.corrs,corr.W[1],corr.W[2])
if(length(cor.WW.b) == 1){phi.WW.b <- cor.WW.b * sqrt(var.W * var.W)} else{
  phi.WW.b <- diag(sqrt(var.W)) %*% cor.WW.b %*% diag(sqrt(var.W))
}

# for binary variables, convert pearson correlations to point-biserial correlations and covariances
binary.W <- rep(0,num.W)
binary.W[power.inputs$mean.W > 0 & power.inputs$mean.W < 1] <- 1
for(r in 1:num.W){
  for(c in 1:num.W){
    if(binary.W[r] == 1 & c != r){
      cor.pointbi <- cor.WW.b[r,c] / sqrt(power.inputs$mean.W[r] * (1 - power.inputs$mean.W[r])) * dnorm(qnorm(power.inputs$mean.W[r]))
      cor.WW.b[r,c] <- cor.WW.b[c,r] <- cor.pointbi
      phi.WW.b[r,c] <- phi.WW.b[c,r] <- cor.WW.b[r,c] * sqrt(phi.WW.b[r,r] * phi.WW.b[c,c])
    }
  }
}

# construct the between-cluster covariance matrix of the Xs and Ws
num.corrs <- num.X * num.W
cor.XW.b <- matrix(runif(num.corrs,corr.X.W[1],corr.X.W[2]), nrow = num.W, ncol = num.X)
if(length(cor.XW.b) == 1){phi.XW.b <- cor.XW.b * sqrt(var.W * var.X.b)} else{
  phi.XW.b <- diag(sqrt(var.W)) %*% cor.XW.b %*% diag(sqrt(var.X.b))
}

# construct the between-cluster covariance matrix
phi.b <- rbind(cbind(phi.XX.b, t(phi.XW.b)), cbind(phi.XW.b, t(phi.WW.b)))

# construct the within-cluster covariance matrix
phi.XWwithXW.w <- phi.XX.w %x% phi.WW.b
phi.XwithXW.w <- matrix(0, nrow = nrow(phi.XWwithXW.w), ncol = ncol(phi.XX.w))
phi.w <- rbind(cbind(phi.XX.w, t(phi.XwithXW.w)), cbind(phi.XwithXW.w, t(phi.XWwithXW.w)))

# solve for the within-cluster regression coefficients
weights.scaled <- 1/sqrt(diag(phi.XX.w)) * weights.X.w
gamma.X.w <- weights.scaled * c(sqrt((var.Y * R2.X.w) / t(weights.scaled) %*% phi.XX.w %*% weights.scaled))

# solve for the cross-level product coefficients
weights.scaled <- 1/sqrt(diag(phi.XWwithXW.w)) * weights.crosslevel.w
if(sum(weights.crosslevel.w) == 0){gamma.XW.w <- weights.crosslevel.w} else {
  gamma.XW.w <- weights.scaled * c(sqrt((var.Y * R2.crosslevel.w) / t(weights.scaled) %*% phi.XWwithXW.w %*% weights.scaled))
}
gamma.w <- c(gamma.X.w,gamma.XW.w)

# random effect correlation matrix
num.corrs <- (num.X + 1) * ((num.X + 1) - 1) / 2
cor.raneffects <- matrix(1, nrow = num.X + 1, ncol = num.X + 1)
corvec <- runif(num.corrs,corr.raneffects[1],corr.raneffects[2])
corindex <- matrix(1:((num.X + 1)*(num.X + 1)), nrow = num.X + 1, ncol = num.X + 1)
cor.raneffects[corindex[upper.tri(corindex)]] <- corvec
cor.raneffects[lower.tri(cor.raneffects)] <- t(cor.raneffects)[lower.tri(t(cor.raneffects))]

# solve for the random slope variances
cor.ranslopes <- cor.raneffects[2:nrow(cor.raneffects),2:nrow(cor.raneffects)]
tau.trace <- (var.Y * R2.ranslopes.w) / sum(diag(cor.ranslopes %*% phi.XX.w %*% diag(weights.ranslopes.w)))
var.ranslopes <- weights.ranslopes.w * tau.trace
if(length(var.ranslopes) == 1){tau.ranslopes <- var.ranslopes} else{
  tau.ranslopes <- diag(sqrt(var.ranslopes)) %*% cor.ranslopes %*% diag(sqrt(var.ranslopes))
}

# compute the within-cluster residual variance
var.e.w <- (1 - icc.Y - R2.X.w - R2.crosslevel.w - R2.ranslopes.w) * var.Y

# solve for the between-cluster coefficients
select.weighted <- matrix(diag(nrow(phi.b))[!is.na(weights.increment.b),], nrow = sum(!is.na(weights.increment.b)))
select.nonweighted <- matrix(diag(nrow(phi.b))[is.na(weights.increment.b),], nrow = sum(is.na(weights.increment.b)))
if(nrow(select.weighted) != nrow(phi.b)){
  phi.nonweighted.b <- select.nonweighted %*% phi.b %*% t(select.nonweighted)
  phi.weighted.b <- select.weighted %*% phi.b %*% t(select.weighted)
  phi.covs.b <- matrix(select.nonweighted %*% phi.b %*% t(select.weighted), nrow = nrow(select.nonweighted), ncol = nrow(select.weighted))
  resvar.W.b <- phi.weighted.b - t(solve(phi.nonweighted.b) %*% phi.covs.b) %*% phi.covs.b
  weights.scaled <- 1/sqrt(diag(resvar.W.b)) * weights.increment.b[!is.na(weights.increment.b)]
  gamma.weighted.b <- weights.scaled * c(sqrt((var.Y * R2.increment.b) / t(weights.scaled) %*% resvar.W.b %*% weights.scaled))
  gamma.b <- c(gamma.w[1:num.X], rep(0,num.W))
  gamma.b[!is.na(weights.increment.b)] <- gamma.weighted.b
  gamma.nonweighted.b <- c(gamma.b[is.na(weights.increment.b)])
} else{
  weights.scaled <- 1/sqrt(diag(phi.b)) * weights.increment.b
  gamma.b <- weights.scaled * c(sqrt((var.Y * R2.increment.b) / t(weights.scaled) %*% phi.b %*% weights.scaled))
}

# compute random intercept variance
tau00 <- var.Y.b - t(gamma.b) %*% phi.b %*% gamma.b

# compute intercept-slope covariance and construct tau matrix
tau <- diag(sqrt(c(tau00, var.ranslopes))) %*% cor.raneffects %*% diag(sqrt(c(tau00, var.ranslopes)))
cor.raneffects[tau == 0] <- 0

# compute fixed intercept and construct coefficient matrix
# the mean of the product from Bohrnstedt & Goldberger Equation 3 simplifies because cov(X.w,W) = 0
means <- c(rep(0,nrow(phi.w)), mean.X, mean.W)
gamma00 <- mean.Y - c(gamma.w, gamma.b) %*% means
gammas <- c(gamma00,gamma.w, gamma.b)

# R-square summary
check.var.Y <- t(gamma.w) %*% phi.w %*% gamma.w + t(gamma.b) %*% phi.b %*% gamma.b + sum(diag(tau[2:nrow(tau),2:nrow(tau)] %*% phi.XX.w)) + tau00 + var.e.w
R2check.X.w <- t(gamma.X.w) %*% phi.XX.w %*% gamma.X.w / check.var.Y
R2check.XW.w <- t(gamma.XW.w) %*% phi.XWwithXW.w %*% gamma.XW.w / check.var.Y
R2check.ranslopes.w <- sum(diag(tau.ranslopes %*% phi.XX.w)) / check.var.Y
R2check.var.e <- ((1 - icc.Y) * var.Y - t(gamma.w) %*% phi.w %*% gamma.w - sum(diag(tau.ranslopes %*% phi.XX.w))) / check.var.Y
R2check.XW.b <- t(gamma.b) %*% phi.b %*% gamma.b / check.var.Y
if(nrow(select.weighted) != nrow(phi.b)){
  R2check.increment.b <- t(gamma.weighted.b) %*% resvar.W.b %*% gamma.weighted.b / check.var.Y
} else{
  R2check.increment.b <- R2check.XW.b
}
R2check.totalminusincrement.b <- R2check.XW.b - R2check.increment.b
R2check.ranicept <- tau00 / check.var.Y

#####################################################################################
# summarize
#####################################################################################

# summarize variance estimates and R2 statistics
var.summary <- matrix(c(var.Y, icc.Y, var.Y.w, var.Y.b, R2check.X.w, R2check.XW.w, R2check.ranslopes.w, R2check.var.e, R2check.XW.b, R2check.increment.b, R2check.totalminusincrement.b,R2check.ranicept), ncol = 1)
row.names(var.summary) <- c("Total Y Variance", "ICC Y", "Within-Cluster Y Variance", "Between-Cluster Y Variance", "Proportion Variance Explained by Within-Cluster Fixed Effects", "Proportion Variance Explained by Cross-Level Interaction Effects", "Proportion Variance Explained by Random Slopes",
                            "Proportion Within-Cluster Error Variance", "Proportion Variance Explained by Between-Cluster Fixed Effects", "Proportion Incremental Variance Explained by Level-2 Predictors", "Proportion Between Variance Inherited from Level-1 Covariates (Between Minus Incremental)", 
                            "Proportion Variance Explained by Random Intercepts")

# collect parameters and construct names
params.coeff <- matrix(c(gammas), ncol = 1)
params.res <- matrix(c(var.e.w), ncol = 1)
vars.Xw <- paste0("X", seq(1:num.X), "w")
vars.Xb <- paste0("X", seq(1:num.X), "b")
vars.W <- paste0("W", seq(1:num.W))
vars.XW <- rep("name", num.X*num.W)
count <- 0
for(x in 1:num.X){
  for(w in 1:num.W){
    count <- count + 1
    vars.XW[count] <- paste0(vars.Xw[x],vars.W[w])
  }}
row.names(tau) <- colnames(tau) <- c("Icept.", paste0(vars.Xw," Slp."))
row.names(params.coeff) <- c("Icept", vars.Xw, vars.XW, vars.Xb, vars.W)
row.names(params.res) <- c("Res. Var.")
colnames(params.coeff) <- colnames(params.res) <- colnames(var.summary) <- "Value"

# focal model parameters
# print(cat(paste("******************************",
#                 "Variance Decomposition Summary",
#                 "******************************", sep = "\n")))
# print(round(var.summary, 4))
# print(cat(paste("**********************************",
#                 "Fixed Effects Coefficients",
#                 "**********************************", sep = "\n")))
# print(round(params.coeff, 4))
# print(cat(paste("**********************************",
#                 "Random Effect Covariance Matrix",
#                 "**********************************", sep = "\n")))
# print(round(tau, 4))
# print(cat(paste("**********************************",
#                 "Random Effect Correlation Matrix",
#                 "**********************************", sep = "\n")))
# print(round(cor.raneffects, 4))
# print(cat(paste("**********************************",
#                 "Within-Cluster Residual Variance",
#                 "**********************************", sep = "\n")))
# print(round(params.res, 4))
# 
MLM.params <- list("Variance Decomposition Summary" = var.summary, "Fixed Effects Coefficients (General Model)" = params.coeff, "Random Effect Covariance Matrix" = tau, "Random Effect Correlation Matrix" = cor.raneffects, "Within-Cluster Residual Variance" = params.res,
                  "Xb Means" = mean.X, "W Means" = mean.W, "Within-Cluster Covariance Matrix of Xw" = phi.XX.w, "Within-Cluster Covariance Matrix of Xw*Wb" = phi.XWwithXW.w, "Between-Cluster Covariance Matrix of Xb and W" = phi.b)

return(MLM.params)
}




