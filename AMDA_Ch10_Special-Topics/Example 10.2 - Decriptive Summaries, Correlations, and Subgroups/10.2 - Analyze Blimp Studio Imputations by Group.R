# EXAMPLE 10.2 - Multiple-Group FCS Multiple Imputation

# remotes::install_github("bkeller2/fdir")

library(fdir)
library(mitml)

# set working directory to the location of this script
set()

# read stacked data
imps <- read.table("./imps/imps.dat", na.strings = "999.0000")
names(imps) <- c("Imputation", "id", "txgrp", "male", "age", "edugroup", "workhrs", "exercise", "paingrps", 
                 "pain", "anxiety", "stress", "control", "depress", "interfere", "disability",
                 paste0("dep", seq(1:7)), paste0("int", seq(1:6)), paste0("dis", seq(1:6)))

implist <- as.mitml.list(split(imps, imps$Imputation))

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