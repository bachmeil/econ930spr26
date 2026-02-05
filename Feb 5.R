library(AER)
data("CreditCard")
attach(CreditCard)

mean(income)
lm(income ~ 1)

lm(income ~ age)
income.dm <- income - mean(income)
age.dm <- age - mean(age)
lm(income.dm ~ age.dm - 1)

R <- matrix(c(-3, 1), nrow=1)
R
q <- 0
beta <- matrix(c(1, 2), ncol=1)
beta
vhat <- matrix(c(1.2, 0.5, 0.5, 0.2), ncol=2)
vhat
outer <- R %*% beta
outer
R %*% vhat %*% t(R)
outer * 1/8 * outer
qchisq(0.95, df=12)
