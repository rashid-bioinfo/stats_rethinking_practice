# ----------------------------------------------------------------#
##-          Confounding Variable - Fork - Example              -##
# ----------------------------------------------------------------#

## ..................................................##
#                 *** Concept ***                 #
" 
  X <- Z -> Y 

"

#                 *** Formula ***                 #
" 
  Di ~ Normal(mu, sigma)
  mu = intercept + slope of people (marriage) + slope of people (age)
  mu = a + b_m * M_i + b_a * A-i
  a ~ Normal(0,10)
  b_m ~ Normal(0,10)
  b_a ~ Normal(0,10)
  sigma ~ Exponential(1)
"
library(rethinking)
n <- 20

a <- rnorm(n,0,0.2)
bM <- rnorm(n,0,0.5)
bA <- rnorm(n,0,0.5)

plot(NULL, xlim=c(-2,2), ylim=c(-2,2),
     xlab='Median age of Marriage (standardized)',
     ylab='Divorced rate (standardized)')

Aseq <- seq(from=-3, to=3, len=30)
for (i in 1:n){
  mu=a[i] + bA[i]*Aseq
  lines(Aseq, mu, lwd=2, col=2)
}

# ----------------------------------------------------------------#
##-                     Analyze the real data                   -##
# ----------------------------------------------------------------#


