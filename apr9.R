library(AER)
data("CreditCard")
attach(CreditCard)

# Two equations in this example
lm(expenditure ~ income + reports)
lm(expenditure ~ income + age)

# Now stack the variables and estimate as a system
N1 <- length(reports)
N2 <- length(age)

# Index of the rows for the two equations
ind1 <- 1:N1
ind2 <- (N1+1):(N1+N2)

y <- c(expenditure, expenditure)
y
x <- matrix(0.0, nrow=N1+N2, ncol=6)

# Fill in the first equation
x[ind1,1] <- 1.0
x[ind1,2] <- income
x[ind1,3] <- reports
head(x)
x[1:100,]

# Fill in the second equation
x[ind2,4] <- 1.0
x[ind2,5] <- income
x[ind2,6] <- age
tail(x)

# Estimate the system of equations
fit.system <- lm(y ~ x - 1)
fit.system  
  
# Now impose that the coefficient on income is the
# same across equations
# Redefine X
# Put income in the same column for both equations
# That imposes the same coefficient across equations
x <- matrix(0.0, nrow=N1+N2, ncol=5)

# Fill in the first equation
x[ind1,1] <- 1.0
x[ind1,2] <- income
x[ind1,3] <- reports
head(x)
x[1:100,]

# Fill in the second equation
x[ind2,4] <- 1.0
# This is the magic
x[ind2,2] <- income
x[ind2,5] <- age
tail(x)

fit.system <- lm(y ~ x - 1)
fit.system

yy <- c(6, 4)
xx <- c(1, 2)
lm(yy ~ xx-1)

fit <- lm(expenditure ~ income + age)
e <- residuals(fit)
plot(e)
e.young <- e[age < median(age)]  
length(e.young)  
e.old <- e[!(age < median(age))]
length(e.old)
sd.young <- sd(e.young)
sd.young
sd.old <- sd(e.old)
sd.old

w <- rep(0.0, 1319)
w[age < median(age)] <- 1/sd.young
w[!(age < median(age))] <- 1/sd.old
exp.star <- expenditure * w
income.star <- income * w
age.star <- age * w

# Now estimated the weighted regression
lm(exp.star ~ w + income.star + age.star - 1)
fit


# Now apply this to the system
# Each equation has its own error variance
y <- c(expenditure, expenditure)
x <- matrix(0.0, nrow=N1+N2, ncol=6)

# Fill in the first equation
x[ind1,1] <- 1.0
x[ind1,2] <- income
x[ind1,3] <- reports

# Fill in the second equation
x[ind2,4] <- 1.0
x[ind2,5] <- income
x[ind2,6] <- age

# Estimate the system of equations on all observations
fit.system <- lm(y ~ x - 1)

# Get the standard deviation of the residual for the first equation
sd.eq1 <- sd(residuals(fit.system)[1:1319])
sd.eq1

# Get the standard deviation of the residual for the second equation
sd.eq2 <- sd(residuals(fit.system)[1320:2638])
sd.eq2

# Set up the weights vector
w <- c(1/rep(sd.eq1, 1319), 1/rep(sd.eq2, 1319))

# Compute the weighted variables for the regression
y.star <- y * w
x.star <- x * w

# Do the weighted regression, remembering to weight the intercept term
# not just the regressors
lm(y.star ~ w + x.star - 1)
