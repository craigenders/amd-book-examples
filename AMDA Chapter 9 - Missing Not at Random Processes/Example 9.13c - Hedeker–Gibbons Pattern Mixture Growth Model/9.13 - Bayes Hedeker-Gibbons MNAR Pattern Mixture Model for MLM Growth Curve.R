# EXAMPLE 9.13a - Bayes Hedeker-Gibbons MNAR Pattern Mixture Model for MLM Growth Curve

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/drugtrial2level.rda"
load(gzcon(url(data_url, open = "rb")))

# pattern mixture model with two dropout groups
analysis1 <- rblimp(
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

output(analysis1)

# pattern mixture model with three dropout groups
analysis2 <- rblimp(
  data = drugtrial2level,
  ordinal = ' earlydrop latedrop',
  clusterid = 'id',
  transform = 'time = sqrt(week)',
  fixed = 'time',
  model = '
      focal.model: 
      severity ~ 1@b0obs time@b1obs drug@b2obs (time*drug)@b3obs earlydrop@b0dif1 (earlydrop*time)@b1dif1 (earlydrop*drug)@b2dif1 (earlydrop*time*drug)@b3dif1 latedrop@b0dif2 (latedrop*time)@b1dif2 (latedrop*drug)@b2dif2 (latedrop*time*drug)@b3dif2 | time; # label coefficients in focal growth model 
      severity ~~ severity@resvar;
      predictor.models: 
      earlydrop ~ 1@edropmean; # use latent response variable means to get pattern proportions 
      latedrop ~ 1@ldropmean', 
  parameters = '
      pedrop = phi(edropmean);  # missingness group proportions
      pldrop = phi(ldropmean);
      pcomp = 1 - pedrop - pldrop;
      cohensd = .10;
      b1dif1 = b1dif2 + cohensd * sqrt(resvar);  # set b1dif1 equal to +.10 residual std. dev. units above b1dif2
      b3dif1 = b3dif2 - cohensd * sqrt(resvar);  # set b3dif1 equal to -.10 residual std. dev. units below b3dif2
      b0 = pcomp * b0obs + pedrop * (b0obs + b0dif1) + pldrop * (b0obs + b0dif2);  # compute weighted average estimates across patterns
      b1 = pcomp * b1obs + pedrop * (b1obs + b1dif1) + pldrop * (b1obs + b1dif2);
      b2 = pcomp * b2obs + pedrop * (b2obs + b2dif1) + pldrop * (b2obs + b2dif2);
      b3 = pcomp * b3obs + pedrop * (b3obs + b3dif1) + pldrop * (b3obs + b3dif2);',
  seed = 90291,
  burn = 10000,
  iter = 10000)

output(analysis2)
