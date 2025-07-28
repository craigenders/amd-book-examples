# example 9.8: bayesian regression pattern mixture model for a diffuse mnar process,

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/drugtrial.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
    data = drugtrial,
    ordinal = 'male drug dropout',
    center = 'severity0 male',
    model = '
      focal.model: # label parameters in focal analysis model 
      severity6 ~ 1@b0obs dropout@b0diff drug@b1obs drug*dropout@b1diff severity0 male; # label residual variance 
      severity6 ~~severity6@resvar;
      predictor.models: # sequential specification for predictors 
      dropout ~ 1@dropmean;
      severity0 male drug ~ dropout;
      auxiliary.variable.models: 
      severity3 severity1 ~ severity6 severity0 drug male', # automatic sequential specification for variables to the left of the tilde 
    parameters = '
      cohensd = .20;
      b0diff = cohensd * sqrt(resvar);  # set b0diff equal to +.20 residual std. dev. units
      b1diff = - cohensd * sqrt(resvar);  # set b1diff equal to -.20 residual std. dev. units
      pobs = 1 - phi(dropmean);  # missingness group proportions
      pmis = phi(dropmean);
      b0mis = b0obs + b0diff;  # compute weighted average intercept and slope across patterns
      b1mis = b1obs + b1diff;
      b0 = (b0obs * pobs) + (b0mis * pmis);
      b1 = (b1obs * pobs) + (b1mis * pmis)',
    seed = 90291,
    burn = 1000,
    iter = 10000)

output(analysis)
