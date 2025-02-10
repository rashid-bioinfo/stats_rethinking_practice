# --------------------------------------------#
##-       Sampling the posterior            -##
# --------------------------------------------#

post_sample <- rbeta(1e3, +1, 3+1)
                    
dens(post_sample, lwd=4, col=2, xlab='proportion of water', adj=0.1)
curve(dbeta(x,6+1,3+1), add=TRUE, lty=2, lwd=3)
