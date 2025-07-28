# example 10.2: multiple-group imputation for gender-specific descriptives

library(mitml)
library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/pain.rda"
load(gzcon(url(data_url, open = "rb")))

impute <- rblimp_fcs(
    data = pain,
    ordinal = 'pain',
    nominal = 'edugroup',
    fixed = 'age edugroup stress control',
    variables = 'age edugroup workhrs exercise pain anxiety stress control interfere depress disability',
    seed = 90291,
    burn = 2000,
    iter = 10000,
    nimps = 100,
    chains = 100) |> by_group('male')

lapply(impute,output)

# mitml list
implist <- as.mitml(impute)

## ---- Step 1. Function to compute stats per imputation ----
compute_group_stats <- function(data) {
  # Keep only numeric variables
  num_data <- data[sapply(data, is.numeric)]
  
  # Split by gender variable 'male'
  groups <- split(num_data, data$male)
  
  # For each group: compute means, SDs, correlations
  lapply(groups, function(g) {
    means <- colMeans(g, na.rm = TRUE)
    sds   <- apply(g, 2, sd, na.rm = TRUE)
    cors  <- cor(g, use = "pairwise.complete.obs")
    list(means = means, sds = sds, cors = cors)
  })
}

## ---- Step 2. Apply to all imputations ----
results <- lapply(implist, compute_group_stats)

## ---- Step 3. Functions to pool means & SDs across imputations ----
pool_means <- function(group_index, varname) {
  mean(sapply(results, function(r) r[[group_index]]$means[varname]))
}

pool_sds <- function(group_index, varname) {
  mean(sapply(results, function(r) r[[group_index]]$sds[varname]))
}

## ---- Step 4. Function to pool correlation matrices ----
pool_cors <- function(group_index) {
  cors_list <- lapply(results, function(r) r[[group_index]]$cors)
  Reduce("+", cors_list) / length(cors_list)
}

## ---- Step 5. Build pooled table for all numeric variables ----
vars <- colnames(implist[[1]][sapply(implist[[1]], is.numeric)])

pooled_table <- do.call(rbind, lapply(vars, function(v) {
  c(var = v,
    mean_male0 = pool_means(1, v),
    sd_male0   = pool_sds(1, v),
    mean_male1 = pool_means(2, v),
    sd_male1   = pool_sds(2, v))
}))

pooled_table <- as.data.frame(pooled_table)

## ---- Step 6. Pooled correlation matrices for each gender ----
pooled_cor_male0 <- pool_cors(1)
pooled_cor_male1 <- pool_cors(2)

## ---- Output ----
pooled_table_rounded <- pooled_table

# Convert all columns except the first one to numeric
pooled_table_rounded[ , -1] <- lapply(pooled_table_rounded[ , -1], function(x) as.numeric(x))

# Now round
pooled_table_rounded[ , -1] <- lapply(pooled_table_rounded[ , -1], function(x) round(x, 3))

print("Pooled Means and SDs by Gender:")
print(pooled_table_rounded, row.names = FALSE)

print("Pooled Correlations (Male = 0):")
print(round(pooled_cor_male0, 3))

print("Pooled Correlations (Male = 1):")
print(round(pooled_cor_male1, 3))
