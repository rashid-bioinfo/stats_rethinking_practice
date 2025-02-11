# ----------------------------------------------------------------#
##-                     Synthetic People                        -##
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
head(dat)
