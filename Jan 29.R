# load third party library
library(AER)

# Load data from AER package
data("CreditCard")

names(CreditCard)
# Don't do this for research
attach(CreditCard)
plot(age, income)
fit <- lm(income ~ age)
fit
# Within the sample, there's a positive relationship
e <- residuals(fit)
sd(e)
length(age)
length(income)
# If we had a different sample from this population
# how much would b change?
e.draw <- rnorm(1319, sd=1.60)
y.draw <- 1.5646 + 0.05422*e.draw
lm(y.draw ~ age)

# But that's only one sample
# Let's create 10,000 samples and get
# b for each sample
set.seed(400)
b.draws <- replicate(10000, {
  e.draw <- rnorm(1319, sd=1.60)
  y.draw <- 1.5646 + 0.05422*e.draw
  fit.draw <- lm(y.draw ~ age)
  coefficients(fit.draw)[2]
})
sd(b.draws)
# What's a 95% interval for b across draws
# from the population?
1.96*sd(b.draws)
-1.96*sd(b.draws)
# If b=0 we expect estimates of b to be
# between -0.000458 and 0.000458.
# Reject H0: b=0
summary(fit)
-1.96*0.0044
1.96*0.0044

quantile(b.draws, probs=c(0.025,0.975))



mean(expenditure)
mean(income)
mean(age)

lm(expenditure ~ income+age)

# FWL theorem
fit.lhs <- lm(expenditure ~ age)
e.lhs <- residuals(fit.lhs)
fit.rhs <- lm(income ~ age)
e.rhs <- residuals(fit.rhs)
lm(e.lhs ~ e.rhs - 1)
lm(expenditure ~ income)
# So does accounting for age offset the change
# in estimated relationship?
# Model selection

summary(lm(expenditure ~ income+age))
# 0.084
summary(lm(expenditure ~ income))
# 0.078
summary(lm(expenditure ~ age))
# negative

AIC(lm(expenditure ~ income+age))
# 18422
AIC(lm(expenditure ~ income))
# 18429
AIC(lm(expenditure ~ age))
# 18538

BIC(lm(expenditure ~ income+age))
# 18443
BIC(lm(expenditure ~ income))
# 18445
BIC(lm(expenditure ~ age))
# 18553