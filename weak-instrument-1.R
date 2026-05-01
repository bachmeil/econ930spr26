# This simulation shows the effect of weak correlation between the instrument
# and the contaminated regressor on the distribution of the 2SLS estimator.
# When you run it, you see that a good instrument is centered on the true
# value, and the variance is small.
# With a weak correlation between the instrument and the contaminated regressor,
# the variance of the estimate gets large, because there's a lot of noise.
# With an instrument that is basically irrelevant (very low correlation
# between the contaminated regressor and the instrument), there is bias and
# a very large variance. You don't learn much from the coefficient of a
# 2SLS regression with a weak instrument.
set.seed(42)
n <- 500
reps <- 5000

# Function to run a simulation with a specific instrument strength
sim_iv <- function(rho) {
  slopes <- replicate(reps, {
    # The regressor (x) is correlated with the erorr term
    # The instrument (z) is correlated with x but not the error term
    e <- rnorm(n)
    u <- 0.8 * e + rnorm(n, sd = sqrt(1 - 0.8^2))
    z <- rnorm(n)
    
    # Generate x so it's correlated with z and u
    x <- rho * z + e 
    y <- 1 * x + u
    
    # Perform 2SLS manually
    x_hat <- fitted(lm(x ~ z))
    coef(lm(y ~ x_hat))[2]
  })
  return(slopes)
}

# Run for different levels of "Strength"
strong <- sim_iv(0.8)   # Strong Instrument
weak   <- sim_iv(0.1)   # Weak Instrument
none   <- sim_iv(0.001) # Effectively No Correlation (The Limit)

# Plot the distribution of estimates
# When the correlation of x and z is strong (0.8), you see that the 2SLS
# estimate is generally close to the correct value (1)
# When the correlation is lower (0.1), the bias is minimal (the peak of the
# distribution is about equal to 1) but the variance is much higher.
# When the correlation is approximately zero, you have a weak instrument problem
# or even an irrelevant instrument problem and the variance is high and
# the peak of the distribution is nowhere close to the true value of 1.
layout(matrix(1:3, nrow = 1))
plot(density(strong), main = "Strong Corr (ID)", col = "black", xlim = c(0.8, 1.2))
abline(v = 1, col = "red", lwd = 2)

plot(density(weak), main = "Weak Corr", col = "black", xlim = c(-6, 8))
abline(v = 1, col = "red", lwd = 2)

plot(density(none), main = "Zero Corr (Limit)", col = "black", xlim = c(-20, 20))
abline(v = 1, col = "red", lwd = 2)
layout(1)