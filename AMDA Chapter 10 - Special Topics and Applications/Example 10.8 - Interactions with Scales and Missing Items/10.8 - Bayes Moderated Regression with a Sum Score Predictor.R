# EXAMPLE 10.8 - Bayes Moderated Regression with a Sum Score Predictor

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/pain.rda"
load(gzcon(url(data_url, open = "rb")))

# basic analysis with sum score as a predictor in an interaction
analysis1 <- rblimp(
  data = pain,
  ordinal = 'dep1:dep7 dis1:dis6',
  fixed = 'male',
  model = 'disability ~ ( dep1:+:dep7 )@b1 male ( (dep1:+:dep7) * male )@b3 pain;', # inline function :+: defines a sum score 
  parameters = '
    slope.female = b1;
    slope.male = b1 + b3',
  seed = 90291,
  burn = 40000,
  iter = 10000)

output(analysis1)

# use all but one dependent variable items as auxiliary variables for its scale score
analysis2 <- rblimp(
    data = pain,
    ordinal = 'dep1:dep7 dis1:dis6',
    fixed = 'male',
    model = '
      focal.model: 
      disability ~ ( dep1:+:dep7 )@b1 male ( (dep1:+:dep7) * male )@b3 pain; # inline function :+: defines a sum score  
      auxiliary.variable.models: 
      dis1:dis5 ~ disability;', # automatic sequential specification using all but one outcome items as auxiliary variables
    parameters = '
      slope.female = b1;
      slope.male = b1 + b3',
    seed = 90291,
    burn = 40000,
    iter = 10000)

output(analysis2)

# add more auxiliary variables
analysis3 <- rblimp(
  data = pain,
  ordinal = 'dep1:dep7 dis1:dis6',
  fixed = 'male',
  model = '
      focal.model: 
      disability ~ ( dep1:+:dep7 )@b1 male ( (dep1:+:dep7) * male )@b3 pain; # inline function :+: defines the sum as a predictor and as part of a product 
      auxiliary.variable.models: 
      dis1:dis5 ~ disability; # automatic sequential specification using all but one outcome items as auxiliary variables
      control interfere ~ disability dep1:+:dep7 male pain;', # automatic sequential specification using all but one outcome items as auxiliary variables
  parameters = '
      slope.female = b1;
      slope.male = b1 + b3',
  seed = 90291,
  burn = 40000,
  iter = 10000)

output(analysis3)
