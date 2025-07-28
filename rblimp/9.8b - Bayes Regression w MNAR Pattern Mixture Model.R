# EXAMPLE 9.8b - Bayes Regression w MNAR Pattern Mixture Model

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/drugtrial.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
    data = drugtrial,
    ordinal = 'male drug dropout',
    center = 'severity0 male',
    model = '
      focal.model: 
      severity6 ~ 1@b0obs dropout@b0diff drug severity0 male; # label parameters in focal analysis model 
      severity6 ~~ severity6@resvar; # label residual variance 
      predictor.models: 
      dropout ~ 1@dropmean; # sequential specification for predictors 
      severity0 male drug ~ dropout;
      auxiliary.variable.models: 
      severity3 severity1 ~ severity6 severity0 drug male', # automatic sequential specification for variables to the left of the tilde 
    parameters = '
      cohensd = .20;
      b0diff = cohensd * sqrt(resvar);  # set b0diff equal to +.20 residual std. dev. units
      pobs = 1 - phi(dropmean);  # missingness group proportions
      pmis = phi(dropmean);
      b0mis = b0obs + b0diff;  # compute weighted average intercept across patterns
      b0 = (b0obs * pobs) + (b0mis * pmis);',
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(analysis)
