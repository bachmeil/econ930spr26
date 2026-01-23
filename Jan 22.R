# Comment
# Consumption growth
# GDP growth
# 2021-2024 annual
gdp <- c(6.15, 2.52, 2.93, 2.79)
cons <- c(13.27, 9.74, 6.46, 5.64)

# cons = a + b gdp + e
# Pair 1: a=1.0 b=2.0
# Pair 2: a=-1.0 b=1.0

# Make predictions
cons1 <- 1.0 + 2.0*gdp
cons2 <- -1.0 + 1.0*gdp

# Plot predictions against actual values
plot(cons1, cons, xlim=c(-2,15), ylim=c(-2,15))
abline(a=0, b=1, lwd=2)
plot(cons2, cons, xlim=c(-2,15), ylim=c(-2,15))
abline(a=0, b=1, lwd=2)

# Appeal to the loss function
# Which set of coefficients has higher loss?
e1 <- cons - cons1
e2 <- cons - cons2
mean(e1^2)
mean(e2^2)

# We'd choose a=1, b=2 if these were our only
# two choices. But they're not...
# Let R choose the best coefficients for us.
# loss is a function of one argument
# Vector with a as first element
# b as second element
loss <- function(coef) {
  pred <- coef[1] + coef[2]*gdp
  # print(pred)
  error <- cons - pred
  return(mean(error^2))
}
loss(c(1.0, 2.0))
loss(c(-1.0, 1.0))
# optim will choose the parameter vector that
# minimizes the loss function
optim(c(1.0, 2.0), loss)
# (2.83, 1.65) is the coefficient vector minimizing
# MSE for our sample
# Use those coefficients to make predictions on
# other samples
lm(cons ~ gdp)




