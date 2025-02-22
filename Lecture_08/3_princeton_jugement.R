#-----------------------------------------------------------------#
##-                       Judgement at Princeton                 -##
# ----------------------------------------------------------------#

"
Si <- Normal(mu, sigma)
mu = Q_w[i]
Q_j <- Normal(0,1)
sigma <- Exponential(1)
"
library(rethinking)
data("Wines2012")

d <- Wines2012

dat <- list(
  S = standardize(d$score),
  J = as.numeric(d$judge),
  W = as.numeric(d$wine),
  X = ifelse(d$wine.amer==1,1,2),
  Z = ifelse(d$judge.amer==1,1,2)
)

mQOJ <- ulam(
  alist(
    S ~ dnorm(mu, sigma),
    mu <- (Q[W] + O[X] - H[J])*D[J] ,
    Q[W] ~ dnorm(0,1),
    O[X] ~ dnorm(0,1),
    H[J] ~ dnorm(0,1),
    D[J] ~ dexp(1),
    sigma ~ dexp(1)
  ), data=dat, chains = 4, cores=4)

precis(mQOJ, 2)
plot(precis(mQOJ, 2))
