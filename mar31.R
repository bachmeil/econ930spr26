library(AER)
data("Affairs")
attach(Affairs)
names(Affairs)
plot(hist(Affairs$affairs))
fm.tobit <- tobit(affairs ~ age + yearsmarried + 
                    religiousness + occupation + rating)
summary(fm.tobit)
occupation
rating
# Expected number of affairs if rel = 3
# Everything else at mean
mean(age) # 32.5
mean(yearsmarried) # 8.2
mean(occupation) # 4.2
mean(rating) # 3.9
plot(hist(rating))
sigma <- 8.247
yhat.ols <- 8.2 - 0.18*32.5 +0.55*8.2 - 1.69*3 +
  0.33*4.2 - 2.28*3.9
yhat.ols
yhat.ols2 <- 8.2 - 0.18*32.5 +0.55*8.2 - 1.69*4 +
  0.33*4.2 - 2.28*3.9
yhat.ols2
# Expected affairs if rel=3
pnorm(yhat.ols/sigma)*yhat.ols + sigma*dnorm(yhat.ols/sigma)
# Expected affairs if rel=4
pnorm(yhat.ols2/sigma)*yhat.ols2 + sigma*dnorm(yhat.ols2/sigma)
