# This simulation shows how the F-stat of the first-stage regression
# indicates the quality of the 2SLS estimator, for the strong
# instrument, kind of weak instrument, and very weak instrument cases.
set.seed(123)
n <- 500
reps <- 1000

# Function to simulate IV and return Beta and First-Stage F
run_sim <- function(rho) {
  # We use replicate to get a matrix where each column is a simulation result
  # Top row = Beta, Bottom row = F-stat
  results <- replicate(reps, {
    u <- rnorm(n)
    v <- 0.8 * u + rnorm(n, sd = sqrt(1 - 0.8^2))
    z <- rnorm(n)
    x <- rho * z + v
    y <- 1 * x + u # True Beta = 1
    
    first_stage <- lm(x ~ z)
    f_stat <- summary(first_stage)$fstatistic[1]
    
    x_hat <- fitted(first_stage)
    beta_iv <- coef(lm(y ~ x_hat))[2]
    
    c(beta_iv, f_stat) 
  })
  return(results) # Returns a matrix
}

# Generate matrices (2 rows by 1000 columns)
strong_mat <- run_sim(rho = 0.5)
weak_mat   <- run_sim(rho = 0.1)
crap_mat   <- run_sim(rho = 0.01)

# Setup plotting
par(mfrow = c(1, 3), mar = c(4.5, 4, 3, 1))

# Helper to plot without column name issues
plot_res <- function(mat, title, color, x_lim) {
  betas <- mat[1, ]
  f_stats <- mat[2, ]
  
  # Filter out extreme outliers, especially for the terrible instrument case
  plot_betas <- betas[betas > x_lim[1] & betas < x_lim[2]]
  
  plot(density(plot_betas), col = color, main = title, 
       xlab = "Estimated Beta", xlim = x_lim)
  abline(v = 1, col = "red", lwd = 2) # True Beta
  mtext(paste("Avg F:", round(mean(f_stats), 1)), side = 3)
}

# Strong instrument: Unbiased and small variance
plot_res(strong_mat, "F >> 10: Strong Instrument", "black", c(0.6, 1.4))

# Kind of weak: Some bias but a bigger variance
plot_res(weak_mat, "F about 5: Sort of weak", "black", c(-1, 4))

# Terrible instrument: Large variance and bias
plot_res(crap_mat, "F < 2: Serious weak instrument", "black", c(-10, 10))
par(mfrow=c(1,1))
