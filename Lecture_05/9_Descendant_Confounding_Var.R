----------------------------------------------------------------#
  ##-                     Elemental Confounds                   -##
  # ----------------------------------------------------------------#
  
  ## ..................................................##
  #                 *** Concept ***                 #
  " 
  Descendant:
  
  X -> Z -> Y, Z -> A
"
library(rethinking)

n <- 1000
X <- rbern(n, 0.5)
Z <- rbern(n, (1-X)*0.1 + X*0.9)
Y <- rbern(n, (1-Z)*0.1 + Z*0.9)
A <- rbern(n, (1-Z)*0.1 + Z*0.9)

cor(X,Y)
cor(X[A==0], Y[A==0])
cor(X[A==1], Y[A==1])
