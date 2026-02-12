# Uniform numbers
# Setting the seed allows replication
set.seed(100)
x <- runif(10000)
x
# Does the histogram have the right shape?
hist(x)
plot(density(x))
?runif

set.seed(100)
y <- rnorm(10000)
plot(density(y))

library(tseries)
jarque.bera.test(y)
# p-value 0.65 => don't reject

# Let's test the properties of the JB test
# How often is the p-value less than 0.05?
set.seed(100)
# Generate 1000 samples
# Collect the p-value for each
output <- replicate(1000, {
  y <- rnorm(10000)
  jarque.bera.test(y)$p.value
})
output
# How many are below 5%?
mean(output < 0.05)
mean(output < 0.10)

# Is OLS unbiased?
set.seed(100)
output <- replicate(1000, {
  x <- rnorm(250)
  e <- rnorm(250)
  y <- x + e
  fit <- lm(y ~ x - 1)
  coefficients(fit)
})
mean(output)
sd(output)
plot(density(output))

# What about a sample of 2500?
set.seed(100)
output <- replicate(1000, {
  x <- rnorm(2500)
  e <- rnorm(2500)
  y <- x + e
  fit <- lm(y ~ x - 1)
  coefficients(fit)
})
mean(output)
sd(output)
plot(density(output))

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







