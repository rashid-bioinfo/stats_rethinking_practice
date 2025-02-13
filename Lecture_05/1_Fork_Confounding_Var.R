# ----------------------------------------------------------------#
##-                     Confounding Variable - Fork              -##
# ----------------------------------------------------------------#

## ..................................................##
#                 *** Concept ***                 #
" 
  X <- Z -> Y 

"
library(rethinking)

n <- 1000

Z <- rbern(n,0.5)
X <- rbern(n, (1-Z)*0.1 + Z*0.9)
Y <- rbern(n, (1-Z)*0.1 + Z*0.9)

df <- data.frame(Z, X, Y)

# Find correlation of X and Y
cor(X,Y)

# Find correlation of X and Y when Z=0 and Z=1
cor(X[Z==0], Y[Z==0])
cor(X[Z==1], Y[Z==1])
