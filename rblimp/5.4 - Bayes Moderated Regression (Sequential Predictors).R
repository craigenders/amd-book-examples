# EXAMPLE 5.4 - Bayes Moderated Regression (Sequential Predictors)

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/pain.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
    data = pain,
    ordinal = 'male pain',
    fixed = 'male',
    center = 'depress',
    model = '
      focal.model: disability ~ depress male depress*male pain;
      predictor.models: depress pain ~ male', # automatic sequential specification for variables to the left of the tilde simple = 'depress | male', 
    simple = 'depress | male', 
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(analysis)
simple_plot(disability ~ depress | male, analysis)
