library(mvtnorm)

missingness.model.function <- function(Rsq.miss, prob.miss, weights, mu, sigma){
  
  # solve for slopes
  weights.scaled <- 1/sqrt(diag(sigma)) * weights
  slopes.miss <- weights.scaled * c(sqrt(((1 - (Rsq.miss / (Rsq.miss - 1))) * Rsq.miss) / t(weights.scaled) %*% sigma %*% weights.scaled))
  
  # iteratively solve for intercept
  N <- 100000
  popdat <- rmvnorm(N, mu, sigma)
  icept.miss.start <- qnorm(prob.miss, 0, 1, lower.tail = T) - mu %*% slopes.miss
  stepsize <- .0001; tol <- .0001; inc <- 0
  repeat{
    inc <- inc - stepsize
    Rstar.Y.hat <- as.numeric(icept.miss.start + inc) + popdat %*% slopes.miss
    mean.prob <- mean(pnorm(0, Rstar.Y.hat, 1, lower.tail = F))
    print(mean.prob)
    if(abs(prob.miss - mean.prob ) <= tol){
      icept.miss <- icept.miss.start + inc
      break
    }
  }

  return(c(icept.miss,slopes.miss))

}


# growth model parameters

# binary predictor
mu.x <- .5
var.x <- mu.x * (1 - mu.x)
# time scores
timescores <- c(0,1,2,3,4)
# regression of growth factors on binary x
gamma <- c(0,0.9212)
# growth factor intercepts
mu.factors <- c(50,0.70711)
# growth factor loading matrix
loadings <- matrix(c(rep(1, length(timescores)), timescores), ncol = 2)
# growth factor variances
sigma.factors <- matrix(c(49.15139,3.32552,3.32552,2.5), ncol = 2)
# within-person residual
sigma.residuals <- diag(length(timescores)) * 41

# model-predicted mean vector and covariance matrix
mu.repeated <- loadings %*% (mu.factors + gamma * mu.x)
mu <- c(mu.x,mu.repeated)
cov.repeated <- loadings %*% (var.x * gamma %*% t(gamma) + sigma.factors) %*% t(loadings) + sigma.residuals
cov.off.diag <- var.x * t(gamma) %*% t(loadings)
sigma <- rbind(cbind(var.x, cov.off.diag),cbind(t(cov.off.diag),cov.repeated))

# adjust covariances for binary variable
sigma.adj <- sigma
target.Rs <- cov2cor(sigma)
for(r in 1:nrow(sigma)){
  for(c in 1:ncol(sigma)){
    if(c != r){
      cor.pointbi <- target.Rs[r,c] / sqrt(mu[1] * (1 - mu[1])) * dnorm(qnorm(mu[1]))
      sigma.adj[r,c] <- sigma.adj[c,r] <- cor.pointbi * sqrt(sigma[r,r] * sigma[c,c])
    }
  }
}

# specify proportional missingness weights and solve for probit coefficients
weights <- c(.5,.5,0,0,0,0)
t3.coeff <- missingness.model.function(Rsq.miss = .25, prob.miss = .10, weights = weights, mu = mu, sigma = sigma.adj)
t4.coeff <- missingness.model.function(Rsq.miss = .25, prob.miss = .20, weights = weights, mu = mu, sigma = sigma.adj)
t5.coeff <- missingness.model.function(Rsq.miss = .25, prob.miss = .40, weights = weights, mu = mu, sigma = sigma.adj)

# missingness model parameters
missingness.regressions <- cbind(t3.coeff,t4.coeff,t5.coeff)
logistic.4.mplus <- gammas * 1.7
