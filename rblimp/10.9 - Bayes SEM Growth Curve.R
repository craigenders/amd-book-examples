# EXAMPLE 10.9 - Bayes SEM Growth Curve

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/drugtrial.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
    data = drugtrial,
    fixed = 'drug',
    latent = 'icepts slopes',
    model = '
      growth.factors: 
      icepts ~ intercept drug; 
      slopes ~ intercept drug;
      icepts ~~ slopes;
      repeated.measures: 
      severity0 ~ intercept@0 icepts@1 slopes@0;
      severity1 ~ intercept@0 icepts@1 slopes@1;
      severity3 ~ intercept@0 icepts@1 slopes@sqrt(3);
      severity6 ~ intercept@0 icepts@1 slopes@sqrt(6);
      severity0~~severity0@resvar;
      severity1~~severity1@resvar;
      severity3~~severity3@resvar;
      severity6~~severity6@resvar', 
    seed = 90291,
    burn = 20000,
    iter = 10000)

output(analysis)
