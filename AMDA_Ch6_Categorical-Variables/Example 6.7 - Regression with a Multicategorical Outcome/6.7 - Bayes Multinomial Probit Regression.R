# EXAMPLE 6.7 - Bayes Multinomial Probit Regression

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/pain.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp_fcs(
    data = pain,
    nominal = 'paingrps',
    fixed = 'control male',
    variables = 'paingrps exercise control male', # FCS imputation gives multinomial probit model for pain,
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(analysis)
