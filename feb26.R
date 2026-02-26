# Set up the MA(1) estimation
# Start with e(0) = 0
# Calculate the errors for a guess of theta0 and theta1
error.calc <- function(theta0, theta1, y, result=0) {
  if (length(y) == 0) { return(result) }
  last.error <- tail(result, 1)
  next.error <- y[1] - theta0 - theta1*last.error
  error.calc(theta0, theta1, y[-1], c(result, next.error))
}
library(AER)
data("USConsump1993")
inc <- USConsump1993[, "income"]
exp <- USConsump1993[, "expenditure"]
error.calc(0, 0.9, exp)

sse.calc <- function(theta0, theta1, y, result=0) {
  if (length(y) == 0) { return(sum(result^2)) }
  last.error <- tail(result, 1)
  next.error <- y[1] - theta0 - theta1*last.error
  sse.calc(theta0, theta1, y[-1], c(result, next.error))
}
sse.calc(0, 0.9, exp)
sse.calc(9250, 0.9, exp)


sse.nls <- function(gamma) {
  income.pow <- inc^gamma
  fit <- lm(exp ~ income.pow)
  return(sum(residuals(fit)^2))
}
sse.nls(1.0)
sse.nls(1.1)
# Consider 0.9 to 1.2 by 0.01
seq(0.9, 1.2, by=0.01)
gamma.sse <- mapply(sse.nls, seq(0.9, 1.2, by=0.01))
which.min(gamma.sse)
seq(0.9, 1.2, by=0.01)[31]
nlsfit <- nls(exp ~ b*inc^g, start=list(b=1, g=0.9))
nlsfit

# Set up a grid for combinations of beta and gamma
potential <- expand.grid(seq(0.7, 0.9, by=0.01),
            seq(0.9, 1.2, by=0.01))

sse.nls <- function(beta, gamma) {
  income.pow <- inc^gamma
  e <- exp - beta*income.pow
  return(sum(e^2))
}
both.sse <- mapply(sse.nls, potential[,1],
                   potential[,2])
both.sse
which.min(both.sse)
potential[245,]

# Interaction model
inc2 <- inc^2
lm(exp ~ inc + inc2)
mean(inc)
# MPC at the mean
0.5 + 0.00002*10175
sd(inc)
0.5 + 0.00002*(10175+2709)
0.5 + 0.00002*(10175-2709)
