# EXAMPLE 5.3 - Bayes Regression

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/employee.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
    data = employee,
    ordinal = 'male',
    fixed = 'male',
    model = 'empower ~ lmx@beta1 climate@beta2 male@beta3', # automatic multivariate distribution for incomplete predictors and latent response scores 
    waldtest = 'beta1:beta3 = 0',
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(analysis)
