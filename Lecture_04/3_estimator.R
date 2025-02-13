# ----------------------------------------------------------------#
##-                     Synthetic People - Estimator              -##
# ----------------------------------------------------------------#

## ..................................................##
#                 *** Formulation ***                 #
" 
  Coding notations: 
  a = alpha
  b = beta
  o = sigma

  Wi ~ Normal (mu, sigma)
  mu = a_s[i] + bHi
  a_j ~ Normal(60,10)
  o ~ Uniform(0,10)
"

library(rethinking)

# Sex: S=1 (for female), S=2 (for male)
sim_HW <- function(S, a, b){
  N <- length(S)
  H <- ifelse(S==1, 150, 160) + rnorm (N,0,5)
  W <- a[S] + b[S]*H + rnorm (N,0,5)
  data.frame(S,H,W)
}

S <- rbern(100) + 1
dat <- sim_HW(S, b=c(0.5,0.6), a=c(0,0))

# estimate posterior

m_SW <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a[S],
    a[S] ~ dnorm(60,10),
    sigma ~ dunif(0,10)
  ), data = dat
)

precis(m_SW, depth = 2)
