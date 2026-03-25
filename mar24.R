library(AER)
data("CreditCard")
attach(CreditCard)

fit.ols <- lm(income ~ age)
fit.ols

# Does MLE give the same answer?
nll <- function(par) {
  a <- par[1]
  b <- par[2]
  sigma <- par[3]
  e <- income - a - b*age
  loglikelihood.obs <- dnorm(e, sd=sigma, log=TRUE)
  loglikelihood.sample <- sum(loglikelihood.obs)
  return(-loglikelihood.sample)
}
nll(c(0,0,1))
nll(c(1,0,1))
# The second parameter vector is a worse choice
optim(c(0,0,1), nll)
names(summary(fit.ols))
summary(fit.ols)$sigma

# MA(1) Estimation
data("USMacroG")
gdp <- USMacroG[,"gdp"]
length(gdp)

# Calculate the next error term
# Use the first value of y
first.error <- function(y, eL1, par) {
  theta0 <- par[1]
  theta1 <- par[2]
  return(y[1] - theta0 - theta1*eL1)
}
first.error(gdp, 0, c(0.5, 0.5))
first.error(gdp[-1], 1610, c(0.5, 0.5))

# Recursion over all elements
all.errors <- function(y, eL1, par) {
  if (length(y) == 0) { stop("We're done") }
  theta0 <- par[1]
  theta1 <- par[2]
  next.error <- y[1] - theta0 - theta1*eL1
  print(next.error)
  return(all.errors(y[-1], next.error, par))
}
all.errors(gdp, 0, c(0.5, 0.5))

# Now modify the function to capture all
# the residuals
all.errors <- function(y, errors, par) {
  if (length(y) == 0) { return(errors) }
  theta0 <- par[1]
  theta1 <- par[2]
  next.error <- y[1] - theta0 - theta1*errors[1]
  return(all.errors(y[-1], c(next.error, errors), par))
}
all.errors(gdp, 0, c(0.5, 0.5))

# Now that we have the error vector, we can evaluate
# the log likelihood
nll.ma1 <- function(par) {
  a <- par[1]
  b <- par[2]
  sigma <- par[3]
  # Change this
  e <- all.errors(gdp, 0, c(a, b))
  loglikelihood.obs <- dnorm(e, sd=sigma, log=TRUE)
  loglikelihood.sample <- sum(loglikelihood.obs)
  return(-loglikelihood.sample)
}
optim(c(0.5, 0.5, 100), nll.ma1)









