mean(x)

predicted.prob <- function(x) {
  return(pnorm(-1.0065 + 0.0444*x))
}
max(income)
min(income)
curve(predicted.prob, from=-100, to=100)
abline(v=0.21, lty=2)
abline(v=13.5, lty=2)

marginal.effect <- function(x) {
  dnorm(-1.0065 + 0.0444*x)*0.0444
}
curve(marginal.effect, from=-100, to=100)
abline(v=0.21, lty=2)
abline(v=13.5, lty=2)

library(AER)
data("USConsump1993")
View(USConsump1993)

inc <- USConsump1993[, "income"]
exp <- USConsump1993[, "expenditure"]

# Calculate the likelihood
# Guess b=0.9
e <- exp - 0.9*inc
# Now get the likelihood for each
lk <- dnorm(e, sd=10)
lk
# log likelihood
llk <- log(lk+1e-12)
llk
lk
sum(llk)



e <- exp - 1.0*inc
# Now get the likelihood for each
lk <- dnorm(e, sd=10)
lk
# log likelihood
llk <- log(lk+1e-12)
llk
lk
sum(llk)

# In practice, maximize that by choice of beta
# Nonlinear model from before
e <- exp - 0.8*inc^(1.02)
lk <- dnorm(e, sd=10)
llk <- log(lk+1e-12)
sum(llk)

# MA(1) model
# Calculating the likelihood function
# Assume normality
data("USMacroG")
gdp <- as.numeric(USMacroG[,"gdp"])
y <- gdp - mean(gdp)
e0 <- 0
theta <- 0.75
e1 <- y[1] - theta*e0
e2 <- y[2] - theta*e1
e3 <- y[3] - theta*e2
e4 <- y[4] - theta*e3

log(dnorm(e1, sd=160))
log(dnorm(e2, sd=160))
log(dnorm(e3, sd=160))
log(dnorm(e4, sd=160))










