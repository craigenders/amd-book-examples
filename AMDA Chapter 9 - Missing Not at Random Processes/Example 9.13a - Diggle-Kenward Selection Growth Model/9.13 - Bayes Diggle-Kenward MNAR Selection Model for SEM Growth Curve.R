# EXAMPLE 9.13 - Bayes Diggle-Kenward MNAR Selection Model for SEM Growth Curve

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/drugtrial.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
    data = drugtrial,
    ordinal = 'sdrop3 sdrop6',
    latent = 'baseline growth',
    model = '
      structural: 
      baseline ~ intercept drug;
      growth ~ intercept drug;
      measurement: 
      severity0 ~ intercept@0 baseline@1 growth@0;
      severity1 ~ intercept@0 baseline@1 growth@1;
      severity3 ~ intercept@0 baseline@1 growth@1.73;
      severity6 ~ intercept@0 baseline@1 growth@2.45;
      severity0 ~~ severity0@resvar;
      severity1 ~~ severity1@resvar;
      severity3 ~~ severity3@resvar;
      severity6 ~~ severity6@resvar;
      selection.model: 
      sdrop3 ~ severity1@mar severity3@mnar; # equality constraints on selection model coefficients 
      sdrop6 ~ severity3@mar severity6@mnar', 
    seed = 90291,
    burn = 100000,
    iter = 100000)

output(analysis)
