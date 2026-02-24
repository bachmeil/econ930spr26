library(AER)
data("USConsump1993")
View(USConsump1993)

inc <- USConsump1993[, "income"]
exp <- USConsump1993[, "expenditure"]

fit.linear <- lm(exp ~ inc)
fit.linear

nlsfit <- nls(exp ~ b*inc^g, start=list(b=1, g=0.9))
nlsfit
summary(nlsfit)

nls.res <- residuals(nlsfit)
nls.fitted <- fitted(nlsfit)
length(exp)
res.sim <- sample(nls.res, size=44, replace=TRUE)
res.sim
expsim <- nls.fitted + res.sim
expsim
coefficients(nlsfit)

# Bootstrap (IID)
set.seed(100)
iidboot <- replicate(1000, {
  # Simulate new expenditure data
  res.sim <- sample(nls.res, size=44, replace=TRUE)
  expsim <- nls.fitted + res.sim
  # Do estimation with new data
  fitsim <- nls(expsim ~ b*inc^g, start=list(b=1, g=0.9))
  # Capture the estimated gamma
  coefficients(fitsim)[2]
})
iidboot
sd(iidboot)
# t-stat is 1.3
# Cannot reject linear model

# Now account for heteroskedasticity
set.seed(100)
hetboot <- replicate(1000, {
  # Simulate new expenditure data
  z <- sample(c(1,-1), size=44, replace=TRUE)
  res.sim <- z*nls.res
  expsim <- nls.fitted + res.sim
  # Do estimation with new data
  fitsim <- nls(expsim ~ b*inc^g, start=list(b=1, g=0.9))
  # Capture the estimated gamma
  coefficients(fitsim)[2]
})
hetboot
sd(hetboot)
# Still not rejecting H0

# Pairs block bootstrap for potential het and serial correlation
# Use tsboot in boot package
bootfnc <- function(ds) {
  simfit <- nls(ds[,"exp"] ~ b*ds[,"inc"]^g,
                start=list(b=1, g=0.9))
  # Now return the new coefficient value
  return(coefficients(simfit)[2])
}
# Load the boot package even though it's installed
library(boot)
dataset <- cbind(inc, exp)
set.seed(100)
blockboot <- tsboot(dataset, bootfnc, 1000, sim="fixed",
                    l=3)
blockboot
# Vector of coeffficient estimates
blockboot$t
sd(blockboot$t)



