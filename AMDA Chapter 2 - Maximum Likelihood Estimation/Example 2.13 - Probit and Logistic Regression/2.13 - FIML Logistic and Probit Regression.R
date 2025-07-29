# EXAMPLE 2.13 - FIML Logistic and Probit Regression

library(aod)

data_url <- "https://raw.githubusercontent.com/craigenders/amd-book-examples/main/Data/employeecomplete.rda"
load(gzcon(url(data_url, open = "rb")))

# logistic regression model with wald test
logitreg <- glm(turnoverint ~ lmx + empower + male, data = employeecomplete, family = "binomial")
summary(logitreg)
wald.test(b = coef(logitreg), Sigma = vcov(logitreg), Terms = 2:4)

# probit regression model with wald test
probitreg <- glm(turnoverint ~ lmx + empower + male, data = employeecomplete, family = "binomial"(link = "probit"))
summary(probitreg)
wald.test(b = coef(probitreg), Sigma = vcov(probitreg), Terms = 2:4)