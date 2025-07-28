# EXAMPLE 10.5 - FCS Multiple Imputation for Mediation w Nested Bootstrap

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(lavaan)
library(mitml)
library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/pain.rda"
load(gzcon(url(data_url, open = "rb")))

impute <- rblimp_fcs(
    data = pain,
    ordinal = 'pain',
    variables = 'anxiety stress control depress interfere pain',
    seed = 90291,
    burn = 10000,
    iter = 10000,
    nimps = 100,
    chains = 100)

output(impute)

# mitml list
implist <- as.mitml(impute)

lavaan_model <- '
  interfere ~ apath*pain
  depress   ~ bpath*interfere + cpath*pain
  indirect := apath*bpath
  total := cpath + (apath*bpath)
'

# fit model with ml and latent response scores
pooled <- sem.mi(lavaan_model, data = implist, estimator = "ml")
summary(pooled, standardized = T, fit = T)

# bootstrap settings
B <- 5   # number of bootstrap samples
param_names <- c("apath", "bpath", "cpath", "indirect", "total")

# storage
boot_estimates <- matrix(NA, nrow = length(implist) * B, ncol = length(param_names))
colnames(boot_estimates) <- param_names

# nested bootstrap over imputations
row_index <- 1
for (m in seq_along(implist)) {
  data_m <- as.data.frame(implist[[m]])
  n <- nrow(data_m)
  
  for (b in 1:B) {
    # draw bootstrap sample
    boot_idx <- sample(seq_len(n), size = n, replace = TRUE)
    boot_data <- data_m[boot_idx, ]
    
    # fit lavaan model to bootstrap sample
    fit <- try(sem(lavaan_model, data = boot_data, se = "none"), silent = TRUE)
    
    if (!inherits(fit, "try-error")) {
      est <- parameterEstimates(fit)
      
      # extract by parameter labels
      boot_estimates[row_index, ] <- c(
        est$est[est$label == "apath"],
        est$est[est$label == "bpath"],
        est$est[est$label == "cpath"],
        est$est[est$label == "indirect"],
        est$est[est$label == "total"]
      )
    }
    
    row_index <- row_index + 1
  }
}

# compute pooled bootstrap estimates & CIs
bootstrap_results <- data.frame(
  parameter = param_names,
  est    = colMeans(boot_estimates, na.rm = TRUE),
  lower  = apply(boot_estimates, 2, quantile, probs = 0.025, na.rm = TRUE),
  upper  = apply(boot_estimates, 2, quantile, probs = 0.975, na.rm = TRUE)
)

print(bootstrap_results)
