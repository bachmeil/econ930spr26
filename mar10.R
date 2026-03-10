library(AER)
data("CreditCard")
attach(CreditCard)

y <- as.integer(reports > 0)
y
x <- income

probit.likelihood <- function(par) {
  # Predict the index value for each observation
  eta <- par[1] + par[2]*x
  # Standard normal CDF
  p <- pnorm(eta)
  
  nll <- -sum(y*log(p) + (1-y)*log(1-p))
  return(nll)
}
mle.solution <- optim(par=c(0,0),
                      probit.likelihood)
mle.solution
# Standard errors
mle.solution <- optim(par=c(0,0),
                      probit.likelihood,
                      hessian=TRUE)
# Built-in function
glm.probit <- glm(y ~ x, family=binomial(link="probit"))
summary(glm.probit)  

solve(mle.solution$hessian)
sqrt(0.007579244)
sqrt(0.0005069415)
mean(x)
# 33,600 is the mean income
# Compare pr(y=1) for 33,600 and 43,600
# Index value is
-1.00652 + 0.04440*3.36
# Now predict the probability of an incident
pnorm(-0.857336)
# New index value is
-1.00652 + 0.04440*4.36
# New predicted probability
pnorm(-0.812936)
# Marginal effect of 10,000 more income starting at the mean
pnorm(-0.812936) - pnorm(-0.857336)
# What about extreme income levels?
sd(x)
# Add 34,000 to get the starting value
-1.00652 + 0.04440*6.8
pnorm(-0.7046)
-1.00652 + 0.04440*7.8
pnorm(-0.6602)
pnorm(-0.6602) - pnorm(-0.7046)
  
-1.00652 + 0.04440*16.8
pnorm(-0.2606)
-1.00652 + 0.04440*17.8
pnorm(-0.2162)

glm.logit <- glm(y ~ x, family=binomial(link="logit"))
summary(glm.logit)
exp(-111)
exp(-5195)
exp(-7515)
exp(-235.32)

objfun <- function(b) {
  y <- c(7, 2, 13)
  x <- c(42, 65, 12)
  e <- y - b*x
  return(-prod(exp(-e^2/2)))
}
optim(0.5, objfun)  
  
  
  