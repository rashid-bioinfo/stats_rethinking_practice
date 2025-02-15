#-----------------------------------------------------------------#
##-                       Good or Bad Control                   -##
# ----------------------------------------------------------------#

library(rethinking)

f <- function(n=100, bXZ=1, bZY=1){
  # X, Z, Y, u
  
  X <- rnorm(n)
  u <- rnorm(n)
  Z <- rnorm(n, bXZ*X + u)
  Y <- rnorm(n, bZY*Z + u)
  bX <- coef(lm(Y~X))['X']
  bXZ <- coef(lm(Y~X+Z))['X']
  return(c(bX, bXZ))
}
test_sim <- replicate(10, f())

sim <- mcreplicate(1e4, f(bZY=0), mc.cores = 8)

dens(sim[1,], lwd=3, xlab='posterior mean')
dens(sim[2,], lwd=3, col=2, add=TRUE)


