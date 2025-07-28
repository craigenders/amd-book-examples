# EXAMPLE 6.3 - Bayes Binary Probit Regression

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/employee.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
    data = employee,
    ordinal = 'turnover male',
    fixed = 'male',
    model = 'turnover ~ lmx empower male', # automatic multivariate distribution for incomplete predictors and latent response scores 
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(analysis)
