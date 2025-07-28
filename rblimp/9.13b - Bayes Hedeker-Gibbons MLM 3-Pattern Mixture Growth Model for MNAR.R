# example 9.13: bayesian hedeker-gibbons mlm 3-pattern mixture growth model for an mnar process,

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/drugtrial2level.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
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
    burn = 1000,
    iter = 10000)

output(analysis)
