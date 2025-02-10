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
# function to calculate wight given height, beta and standard deviation

sim_weight <- function(H, b, sd){
  U <- rnorm(length(H), 0, sd)
  W <- b*H + U
  return(W)
}


H <- runif(10,130,170)
W <- sim_weight(H,b=0.5,sd=5)

# Run the model
library(rethinking)

m3.1 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + (b*H),
    a ~ dnorm(0,10),
    b ~ dunif(0,1),
    sigma ~ dunif(0,10)
  ),data=list(W=W,H=H)
)

# summary
precis(m3.1)
