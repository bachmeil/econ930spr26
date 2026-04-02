library(tstools)
pce <- import.fred("pce.csv")
start(pce)
tbill <- import.fred("tbill.csv")
inflation <- import.fred("inflation.csv")
dip <- import.fred("dip.csv")
consratio <- lags(pce, -1)/pce

beta <- 0.9
gamma <- 1.5
dev <- (beta*(consratio)^(-gamma))*tbill-1
plot(dev)

moment1 <- dev*inflation
moment2 <- dev*dip
plot(moment1)
plot(moment2)
mean(na.omit(as.numeric(moment1)))
mean(na.omit(as.numeric(moment2)))

objfun <- function(par) {
  beta <- par[1]
  gamma <- par[2]
  dev <- (beta*(consratio)^(-gamma))*tbill-1
  moment1 <- mean(na.omit(as.numeric(dev*inflation)))
  moment2 <- mean(na.omit(as.numeric(dev*dip)))
  return(moment1^2 + moment2^2)
}
objfun(c(0.9,1.5))
optim(c(0.9, 1.5), objfun)

library(quantreg)
data(barro)
attach(barro)
hist(y.net)
lm(y.net ~ Iy2)
rq(y.net ~ Iy2, tau=0.5)
rq(y.net ~ Iy2, tau=0.9)
summary(rq(y.net ~ Iy2, tau=0.1))

# industrial production growth data
# arima is built into R
ar1 <- arima(dip, order=c(1,0,0))
ar1
plot(dip)
# Take the first difference
dip.diff <- diff(dip)
# AR(3) model
ar3 <- arima(dip.diff, order=c(3,0,0))
ar3
predict(ar3, n.ahead=6)

