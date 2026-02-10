library(AER)
data("CreditCard")
attach(CreditCard)
Y <- majorcards
mean(Y)

## Implement OLS directly
## Construct the regressor matrix X
X <- cbind(1, age, dependents)
X
dim(X)

## solve takes the inverse
## t takes the transpose of a matrix
## This is betahat ols
solve(t(X) %*% X) %*% t(X) %*% Y

## Built-in OLS command (safer than the manual version above)
lm(majorcards ~ age+dependents)
