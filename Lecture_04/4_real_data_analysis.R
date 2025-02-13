# ----------------------------------------------------------------#
##-                     Analyze the real sample                 -##
# ----------------------------------------------------------------#

library(rethinking)
data("Howell1")

d2 <- Howell1

d2 <- d2[d2$age>18, ]

dat <- list(
  W = d2$weight,
  S = d2$male + 1 ) # S=2 (female), S=1 (male)


m_SW <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a[S],
    a[S] ~ dnorm(60,10),
    sigma ~ dunif(0,10)
  ), data=dat)
  
  precis(m_SW)

  # posterior mean W
  post <- extract.samples(m_SW)
  dens(post$a[,1], xlim=c(39,50), lwd=3,
       col=2, xlab="posterior mean weight (kg)")
  dens(post$a[,2], lwd=3, col=4, add=TRUE)

  # posterior W distribution
  W1 <- rnorm(1000, post$a[,1], post$sigma)
  W2 <- rnorm(1000, post$a[,2], post$sigma)
  dens(W1, xlim=c(20,70), ylim=c(0,0.085),
       lwd=3, col=2)
  dens(W2, lwd=3, col=4, add=TRUE)

# Contrast
W_constrast <- W2 - W1
dens(W_constrast, xlim=c(-25,35), lwd=3, col=1, xlab="Posterior weight contrast (kg)")  

# proportion above zero
sum(W_constrast > 0) /1000
# proportion below zero
sum(W_constrast < 0) /1000
