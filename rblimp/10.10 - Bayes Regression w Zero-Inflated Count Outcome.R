# EXAMPLE 10.10 - Bayes Regression w Zero-Inflated Count Outcome

# requires blimp installation from www.appliedmissingdata.com/blimp
# remotes::install_github('blimp-stats/rblimp')
# remotes::update_packages('rblimp')
library(rblimp)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/alcoholuse.rda"
load(gzcon(url(data_url, open = "rb")))

analysis <- rblimp(
    data = alcoholuse,
    ordinal = 'college male alcdays_bin',
    count = 'alcdays_cnt',
    transform = '
      alcdays_cnt = missing(alcdays == 0, alcdays); # missing if drinking days = 0, # of drinking days otherwise
      alcdays_bin = ifelse(alcdays == 0, 0, 1)', # 0 = no drinks, 1 = one or more drinking days
    fixed = 'male',
    model = '
      alcdays_bin ~ alcage college age male; # 0 vs. 1 model
      alcdays_cnt ~ alcage college age male', # non-zero model 
    seed = 90291,
    burn = 10000,
    iter = 10000)

output(analysis)
