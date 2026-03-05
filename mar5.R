library(AER)
data("CreditCard")
attach(CreditCard)
reports

y <- factor(reports > 0)
y
x <- income
# Does y fall with income?
probit.fit <- glm(y ~ x,
  family=binomial(link="probit"))
probit.fit
coef(probit.fit)
summary(probit.fit)
