# ----------------------------------------------------------------#
##-    Prior Predictive Simulation - Simulation of 10 people    -##
# ----------------------------------------------------------------#

## ..................................................##
#                 *** Formulation ***                 #
" 
  Coding notations: 
  a = alpha
  b = beta
  o = sigma

  Wi ~ Normal (mu, sigma)
  mu = a + bHi
  a ~ Normal(0,10)
  b ~ Uniform(0,1)
  o ~ Uniform(0,10)
"

# Run the model
library(rethinking)
data("Howell1")

d2 <- Howell1

dat <- list(W=d2$weight, H=d2$height)

m3.1 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + (b*H),
    a ~ dnorm(0,10),
    b ~ dunif(0,1),
    sigma ~ dunif(0,10)
  ),data=dat
)

# summary
precis(m3.1)
