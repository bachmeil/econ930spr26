library(AER)
data("CreditCard")
attach(CreditCard)
fit <- lm(majorcards ~ age+dependents)
fit
names(fit)

# Regression model summary statistics
fit.sum <- summary(fit)
names(fit.sum)
# Coefficient cov matrix
vhat <- vcov(fit)
# Create a 3x1 matrix the beta estimates
bhat <- matrix(coefficients(fit), ncol=1)
vhat
bhat

# Create a 3x3 identity matrix
R <- diag(3)
R

# Create a 3x1 matrix with the beta 
# values under H0
q <- matrix(0, nrow=3, ncol=1)
q

outer <- R %*% bhat - q
outer
inner <- R %*% vhat %*% t(R)
# Wald test statistic
t(outer) %*% solve(inner) %*% outer
# 95% critical value
qchisq(0.95, df=3)
