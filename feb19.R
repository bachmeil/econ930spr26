# Iterative solution
Dinv <- matrix(c(1/3, 0, 0, 0.25), ncol=2)
Dinv
N <- matrix(c(0,2,1,0), ncol=2)
N
b <- c(6,10)
b
T <- -Dinv %*% N
T
C <- Dinv %*% b
C

# Initial guess
guess <- c(1,1)

# Iterate on your guess until convergence
for (ii in 1:30) {
  guess <- T %*% guess + C
  print(guess)
}

# We've converged to the solution
# 3x + y = 6
3*1.4+1.8
# 2x+4y=10
2*1.4 + 4*1.8


# What if the convergence condition is not met?
Dinv <- matrix(c(-10, 0, 0, 25), ncol=2)
Dinv
N <- matrix(c(0,2,1,0), ncol=2)
N
b <- c(6,10)
b
T <- -Dinv %*% N
T
C <- Dinv %*% b
C

# Initial guess
guess <- c(1,1)

# Iterate on your guess until convergence
for (ii in 1:30) {
  guess <- T %*% guess + C
  print(guess)
}
solve(Dinv)

x <- c(1.1, 2.2, 3.3, 4.4, 5.5)
y <- c(0.3, -0.6, 0.7, -0.8, -0.9)
z <- c(2.5, -.25, 2.5, -.25, 2.5)
fit <- lm(y ~ x)
fit
fit2 <- lm(y ~ x+z)
fit2
sqrt(0.2667)

library(AER)
data("USConsump1993")
View(USConsump1993)

inc <- USConsump1993[, "income"]
exp <- USConsump1993[, "expenditure"]
plot(exp)

# OLS
lm(exp ~ inc)

# If nls is an option
nlsfit <- nls(exp ~ b*inc^g, start=list(b=1, g=0.9))
nlsfit

# General approach
objfnc < function(par) {
  b <- par[1]
  g <- par[2]
  exp.hat <- b*inc^g
  e <- exp - exp.hat
  sse <- sum(e^2)
  return(sse)
}
objfnc(c(0.3, 1.2))
optim(c(1, 0.9), objfnc)


















