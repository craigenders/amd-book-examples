# EXAMPLE 10.2 - Multiple-Group FCS Multiple Imputation

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

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
    burn = 10000,
    iter = 10000,
    nimps = 100,
    chains = 100) |> by_group('male')

lapply(impute,output)

# mitml list
implist <- as.mitml(impute)

# step 1. function to compute stats per imputation
compute_group_stats <- function(data) {
  # keep only numeric variables
  num_data <- data[sapply(data, is.numeric)]
  
  # split by gender variable 'male'
  groups <- split(num_data, data$male)
  
  # for each group: compute means, SDs, correlations
  lapply(groups, function(g) {
    means <- colMeans(g, na.rm = TRUE)
    sds   <- apply(g, 2, sd, na.rm = TRUE)
    cors  <- cor(g, use = "pairwise.complete.obs")
    list(means = means, sds = sds, cors = cors)
  })
}

# step 2. apply to all imputations
results <- lapply(implist, compute_group_stats)

# step 3. functions to pool means & SDs across imputations
pool_means <- function(group_index, varname) {
  mean(sapply(results, function(r) r[[group_index]]$means[varname]))
}

pool_sds <- function(group_index, varname) {
  mean(sapply(results, function(r) r[[group_index]]$sds[varname]))
}

# step 4. function to pool correlation matrices
pool_cors <- function(group_index) {
  cors_list <- lapply(results, function(r) r[[group_index]]$cors)
  Reduce("+", cors_list) / length(cors_list)
}

# step 5. build pooled table for all numeric variables
vars <- colnames(implist[[1]][sapply(implist[[1]], is.numeric)])

pooled_table <- do.call(rbind, lapply(vars, function(v) {
  c(var = v,
    mean_male0 = pool_means(1, v),
    sd_male0   = pool_sds(1, v),
    mean_male1 = pool_means(2, v),
    sd_male1   = pool_sds(2, v))
}))

pooled_table <- as.data.frame(pooled_table)

# step 6. pooled correlation matrices for each gender
pooled_cor_male0 <- pool_cors(1)
pooled_cor_male1 <- pool_cors(2)

# output
pooled_table_rounded <- pooled_table

# convert all columns except the first one to numeric
pooled_table_rounded[ , -1] <- lapply(pooled_table_rounded[ , -1], function(x) as.numeric(x))

# round and print
pooled_table_rounded[ , -1] <- lapply(pooled_table_rounded[ , -1], function(x) round(x, 3))

print("Pooled Means and SDs by Gender:")
print(pooled_table_rounded, row.names = FALSE)

print("Pooled Correlations (Male = 0):")
print(round(pooled_cor_male0, 3))

print("Pooled Correlations (Male = 1):")
print(round(pooled_cor_male1, 3))
