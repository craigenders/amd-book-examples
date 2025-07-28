# EXAMPLE 9.13a - Bayes Hedeker-Gibbons MNAR Pattern Mixture Model for MLM Growth Curve

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/drugtrial2level.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
    data = drugtrial2level,
    ordinal = ' drug dropout',
    clusterid = 'id',
    transform = 'time = sqrt(week)',
    fixed = 'time',
    model = '
      focal.model: severity ~ 1@b0obs time@b1obs drug@b2obs (time*drug)@b3obs dropout@b0dif (dropout*time)@b1dif (dropout*drug)@b2dif (dropout*time*drug)@b3dif | time; # label coefficients in focal growth model 
      predictor.models: 
      dropout ~ 1@dropmean; # sequential specification for predictors 
      drug ~ dropout', 
    parameters = '
      pobs = 1 - phi(dropmean); # missingness group proportions
      pmis = phi(dropmean);
      b0 = pobs * b0obs + pmis * (b0obs + b0dif); # compute weighted average estimates across patterns
      b1 = pobs * b1obs + pmis * (b1obs + b1dif);
      b2 = pobs * b2obs + pmis * (b2obs + b2dif);
      b3 = pobs * b3obs + pmis * (b3obs + b3dif)',
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(analysis)
