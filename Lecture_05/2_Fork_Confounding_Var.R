# ----------------------------------------------------------------#
##-                     Confounding Variable - Fork              -##
# ----------------------------------------------------------------#

## ..................................................##
#                 *** Concept ***                 #
" 
  X <- Z -> Y 

"
library(rethinking)

cols = c(4,2)

N <- 300 
Z <- rbern(N)
X <- rnorm(N, 2*Z-1)
Y <- rnorm(N, 2*Z-1)

df <- data.frame(Z, X, Y)

plot(X,Y, col=cols[Z+1], lwd=3)

abline(lm(Y[Z==1]~X[Z==1]), col=2, lwd=3)
abline(lm(Y[Z==0]~X[Z==0]), col=3, lwd=3)
abline(lm(Y~X), lwd=3)
