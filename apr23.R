# Price and quantity reflect shifts of
# both curves
# Note: AI-generated code that was edited
set.seed(42)
n_periods <- 8  # Keep it small so the lines are visible

# Function to plot shifting curves and their intersections
plot_identification <- function(s_sd, d_sd, main_title) {
  # Base plot setup
  plot(NULL, xlim = c(0, 20), ylim = c(0, 20), 
       xlab = "Quantity", ylab = "Price", main = main_title)
  
  # Generate shifts
  s_shifts <- rnorm(n_periods, 0, s_sd)
  d_shifts <- rnorm(n_periods, 20, d_sd)
  
  # Containers for intersection points
  q_points <- numeric(n_periods)
  p_points <- numeric(n_periods)
  
  for(i in 1:n_periods) {
    # Supply: P = Q + shift (Slope = 1)
    # Demand: P = shift - Q (Slope = -1)
    
    # Intercepts
    s_int <- s_shifts[i]
    d_int <- d_shifts[i]
    
    # Draw the curves (Supply in green, Demand in blue)
    # Using light colors so the points stand out
    abline(a = s_int, b = 1, lwd = 1)
    abline(a = d_int, b = -1, col = "blue", lwd = 1)
    
    # Calculate Intersection (Q + s = d - Q  => 2Q = d - s)
    q_points[i] <- (d_int - s_int) / 2
    p_points[i] <- q_points[i] + s_int
  }
  
  # Plot the equilibrium points (what the researcher actually sees)
  points(q_points, p_points, pch = 19, col = "red", cex = 1.2)
  
  # Add the estimated regression line through the red points
  if(n_periods > 1) {
    #abline(lm(p_points ~ q_points), col = "black", lwd = 2, lty = 2)
  }
}

plot_identification(s_sd = 3, d_sd = 3, "Both Curves Shifting")
plot_identification(s_sd = 5, d_sd = 0.4, "Mostly Supply Shocks")
plot_identification(s_sd = 0.4, d_sd = 5, "Mostly Demand Shocks")

# 1000 observations under each of the
# above scenarios
# Note: AI-generated code that was edited
set.seed(123)
n <- 1000

# Function to simulate and plot
sim_market <- function(var_s, var_d, title) {
  # Structural Shocks
  # Supply: P = Q + e_s
  # Demand: P = 20 - Q + e_d
  e_s <- rnorm(n, mean = 0, sd = sqrt(var_s))
  e_d <- rnorm(n, mean = 0, sd = sqrt(var_d))
  
  # Solve for Equilibrium:
  # Q + e_s = 20 - Q + e_d
  # 2Q = 20 + e_d - e_s
  q <- 10 + (e_d - e_s) / 2
  p <- q + e_s
  
  # Plotting the "Data Cloud"
  plot(q, p, pch = 16, 
       main = title, xlab = "Quantity", ylab = "Price",
       xlim = c(5, 15), ylim = c(5, 15))
  
  # Add the OLS Regression Line (The "Estimated" Curve)
  #model <- lm(p ~ q)
  #abline(model, col = "red", lwd = 3)
  
  # Add text for the estimated slope
  #slope <- round(coef(model)[2], 2)
  #text(5, 18, paste("Slope:", slope), col = "red", font = 2)
}

sim_market(var_s = 1, var_d = 1, "Equal Variance")
sim_market(var_s = 1, var_d = 9, "Mostly Demand Shocks")
sim_market(var_s = 9, var_d = 1, "Mostly Supply Shocks")

# Now put 2SLS into practice
# Return to the Kmenta example
library(systemfit)
data("Kmenta")
attach(Kmenta)
Kmenta

# First stage regression
# Narrow the data to exogenous movements in price
fit1 <- lm(price ~ income + farmPrice + trend)
price.hat <- fitted(fit1)
# Second stage regression
# Narrow the data further
# price.hat captures supply shifts
# Review the FWL Theorem if you don't
# remember why
fit2.demand <- lm(consump ~ price.hat + income)
fit2.demand
fit2.supply <- lm(consump ~ price.hat + farmPrice + trend)
fit2.supply

# Using systemfit
# Gives correct standard errors and correct inference
eqDemand <- consump ~ price + income
eqSupply <- consump ~ price + farmPrice + trend
system <- list(demand=eqDemand, supply=eqSupply)
inst <- ~ income + farmPrice + trend
fit.2sls <- systemfit(system, "2SLS", inst=inst)
fit.2sls
summary(fit.2sls)
vcov(fit.2sls)
