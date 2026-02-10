library(AER)
data("CreditCard")
attach(CreditCard)
Y <- majorcards
mean(Y)
X <- cbind(1, age, dependents)
X
dim(X)
solve(t(X) %*% X) %*% t(X) %*% Y
lm(majorcards ~ age+dependents)
