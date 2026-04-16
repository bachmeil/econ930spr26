library(systemfit)
data("Kmenta")
attach(Kmenta)

# Do OLS to get the residuals
ols1 <- lm(consump ~ price + income)
ols2 <- lm(consump ~ price + farmPrice + trend)
resids <- cbind(resid(ols1), resid(ols2))

# Residual covariance matrix
# Sigma term in our notation on the board
N <- length(consump)
Sigma <- (t(resids) %*% resids)/N
Sigma
# Covariance of the residuals in the same year
# Stack the variable
y.stacked <- c(consump, consump)
X1 <- model.matrix(ols1)
head(X1)
X2 <- model.matrix(ols2)
library(Matrix)
x.stacked <- as.matrix(bdiag(X1, X2))
head(x.stacked)

# Compute the weighting matrix
omega.inv <- solve(Sigma) %x% diag(N)
head(omega.inv)

# Now plug into the GLS formula to get your SUR estimates
beta.sur <- solve(t(x.stacked) %*% omega.inv %*% x.stacked) %*%
  t(x.stacked) %*% omega.inv %*% y.stacked
beta.sur

# Compare with OLS
ols1

eqDemand <- consump ~ price + income
eqSupply <- consump ~ price + farmPrice + trend
system <- list(demand = eqDemand,
               supply = eqSupply)
system

fit.sur <- systemfit(system, method="SUR", data=Kmenta)
summary(fit.sur)
Sigma

fit.ols <- systemfit(system, method="OLS",
                     data=Kmenta)
fit.ols
