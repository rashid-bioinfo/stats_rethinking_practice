----------------------------------------------------------------#
  ##-                     Elemental Confounds                   -##
  # ----------------------------------------------------------------#
  
  ## ..................................................##
  #                 *** Concept ***                 #
  " 
  Pipe:
  
  X -> Z -> Y 
  
  "
library(rethinking)

cols <- c(4,2)

N <- 300
X <- rnorm(N)
Z <- rbern(N, inv_logit(X))
Y <- rnorm(N, (2*Z-1))

plot(X,Y, col=cols[Z+1], lwd=3)

abline(lm(X[Z==0] ~ Y[Z==0]), col=2, lwd=3)
abline(lm(X[Z==1] ~ Y[Z==1]), col=4, lwd=3)
abline(lm(X~Y), lwd=3)
