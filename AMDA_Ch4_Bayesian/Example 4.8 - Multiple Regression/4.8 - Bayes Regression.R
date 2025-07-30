# EXAMPLE 4.8 - Bayes Regression

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/smokingcomplete.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
    data = smokingcomplete,
    fixed = 'parsmoke age income',
    center = 'age income',
    model = 'intensity ~ parsmoke@beta1 age@beta2 income@beta3', 
    waldtest = 'beta1:beta3 = 0',
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(analysis)
