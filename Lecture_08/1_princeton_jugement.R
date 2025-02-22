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

mQ <- ulam(
  alist(
    S ~ dnorm(mu, sigma),
    mu <- Q[W],
    Q[W] ~ dnorm(0,1),
    sigma ~ dexp(1)
  ), data=dat, chains = 4, cores=4)

precis(mQ, 2)
