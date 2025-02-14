----------------------------------------------------------------#
  ##-                     Elemental Confounds                   -##
  # ----------------------------------------------------------------#
  
  ## ..................................................##
  #                 *** Concept ***                 #
  " 
  Collider:
  
  X -> Z <- Y 
  
  "
library(rethinking)

cols = c(4,2)

N <- 300
X <- rnorm(N)
Y <- rnorm(N)
Z <- rbern(N, inv_logit(2*X+2*Y-2))

plot(X, Y, col=cols[Z+1], lwd=3)

abline(lm(Y[Z==0]~X[Z==0]), col=2, lwd=3)
abline(lm(Y[Z==1]~X[Z==1]), col=4, lwd=3)
abline(lm(X~Y), lwd=3)     
