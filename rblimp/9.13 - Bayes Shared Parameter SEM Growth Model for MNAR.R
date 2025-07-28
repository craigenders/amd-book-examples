# example 9.13: bayesian shared parameter mlm growth model for an mnar process,

library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/drugtrial.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
    data = drugtrial,
    ordinal = 'drug sdrop3 sdrop6',
    latent = 'baseline growth',  # define random intercept and slope latent variables',
    model = '
      latent.model: 
      baseline ~ 1 drug;
      growth ~ 1 drug;
      baseline ~~ growth;
      measurement.model: 
      severity0 ~ 1@0 baseline@1 growth@0;
      severity1 ~ 1@0 baseline@1 growth@1;
      severity3 ~ 1@0 baseline@1 growth@1.73;
      severity6 ~ 1@0 baseline@1 growth@2.45;
      severity0 ~~ severity0@resvar;
      severity1 ~~ severity1@resvar;
      severity3 ~~ severity3@resvar;
      severity6 ~~ severity6@resvar;
      missingness.model: 
      sdrop3 ~ baseline@icept growth@slope drug@tx;
      sdrop6 ~ baseline@icept growth@slope drug@tx', 
    seed = 90291,
    burn = 50000,
    iter = 10000)

output(analysis)
