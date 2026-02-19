library(AER)
data("CreditCard")
attach(CreditCard)
fit <- lm(income ~ age)

# Simulate from this process
# Derive the covariance matrix of the coefficients
# age is normally distributed
mean(age)
sd(age)
e <- residuals(fit)
sd(e)

set.seed(100)
output <- replicate(1000, {
  # Generate age
  age <- rnorm(250, mean(age), sd=sd(age))
  # Generate the error
  error <- rnorm(250, sd=sd(e))
  # Have to generate income
  income <- 1.56 + 0.054*age + error
  fit <- lm(income ~ age)
  coefficients(fit)
})
dim(output)
# Properties of the intercept
mean(output[1,])
sd(output[1,])
# Properties of the slope coefficient
mean(output[2,])
sd(output[2,])
# Calculate the covariance matrix
cov(output[1,], output[2,])
cov(t(output))
vcov(fit)

set.seed(100)
fit <- lm(income ~ age)
e <- residuals(fit)
fit
length(e)
# Generate a new residual series
# Sample residuals with replacement
e.sim <- sample(e, size=1319, replace=TRUE)
e.sim
# Generate new income
income.sim <- fitted(fit) + e.sim
# Now we have a new sample of income
# Taken from the population
# Now get a second set of coefficients
lm(income.sim ~ age)
fit

set.seed(100)
output <- replicate(1000, {
  e.sim <- sample(e, size=1319, replace=TRUE)
  income.sim <- fitted(fit) + e.sim
  fit <- lm(income.sim ~ age)
  coefficients(fit)
})
cov(t(output))
vcov(fit)

# Flip the sign
z <- sample(c(1,-1), size=1319, replace=TRUE)
z
# New residual vector from that population
z*e


# Do the wild bootstrap to correct for possible
# heteroskedasticity
set.seed(100)
output <- replicate(1000, {
  z <- sample(c(1,-1), size=1319, replace=TRUE)
  e.sim <- z*e
  income.sim <- fitted(fit) + e.sim
  fit <- lm(income.sim ~ age)
  coefficients(fit)
})
cov(t(output))
vcov(fit)
library(boot)
tsboot



