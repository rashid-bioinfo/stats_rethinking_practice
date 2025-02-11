# --------------------------------------------#
##- Design Bayesian Estimator of the sample -##
# --------------------------------------------#


# Formula used:
# Ways for p to produce W,L = (4p)^W x (4-4p)^L

sample <- c('W','L','W','W','W','L','W','L','W')
W <- sum(sample=='W')
L <- sum(sample=='L')
# p: proportion of water in the sample
p= c(0,0.25,0.5,0.75,1)
ways <- sapply(p, function(q) (q*4)^W * (4*(1-q))^L )
# prob: probability of proportion - It is calculating each individual value by the sum of all values in the sample

prob <- ways/sum(ways)

cbind(p,ways,round(p,2))

# --------------------------------------------#
##-          Test  Bayesian Estimator       -##
# --------------------------------------------#

# Formula used: W, L = f(p,N)
globe_sim <- function(p=0.5,N=9){
  sample(c('W','L'), size=N, prob=c(p,1-p), replace=TRUE)
}

globe_sim()

# replicate the simulations for 10 times
replicate(n=10, globe_sim())

# test the model with extreme settings

globe_sim(p=1.0, N=10)

globe_sim(p=0, N=10)

# to check if W is found half of the time when running simulations for 10,000 times
sum(globe_sim(p=0.5,N=1e4) == 'W') / 1e4

# -----------------------------------------------#
##- Simulate posterior predictive distribution -##
# -----------------------------------------------#

post_sample <- rbeta(1e4, 6+1,3+1)

post_prediction <- sapply(post_sample, function(p) sum(globe_sim(p,10) == 'W'))

tab_post <- table(post_prediction)

#for (i in 0:10) lines(c(i,i), c(0,tab_post[i+1], lwd=4, col=4))
 
for (i in 0:10) lines(c(i,i), c(0, tab_post[i+1]), lwd=4, col=4)

# --------------------------------------------#
##-         Misclassified Simulation        -##
# --------------------------------------------#

# Obey the workflow - Code a generative model

globe_sim2 <- function(p=0.7, N=9, x=0.1){
  true_sample <- sample(c('W', 'L'), size=N, prob=c(p,1-p), replace=TRUE)
  obs_sample <- ifelse(runif(N) < x,
                       ifelse(true_sample == 'W', 'L', 'W'), # error
                      true_sample) # no error
  return(obs_sample)
}