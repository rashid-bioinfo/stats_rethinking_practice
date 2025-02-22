#-----------------------------------------------------------------#
##-           Checking gender biases in Berkely                -##
# ----------------------------------------------------------------#
"

G --> A
G --> D --> A

"

library(rethinking)

# Generative model, basic mediator scenario
N <- 1000 # number of applications
# Even gender distribtuion
G <- sample(1:2, size=N, replace=TRUE) 
# Gender 1 tends to apply to department 1, 2 to 2
D <- rbern(N, ifelse(G==1, 0.3, 0.8)) + 1
# Matrix of acceptance rates [dept, gender]
acceptance_rate <- matrix(c(0.1,0.3,0.1,0.3), nrow=2)
# Simulate acceptance
A <- rbern(N, acceptance_rate[D,G])

## - Total gender effect

dat_sim <- list(A=A, D=D, G=G)

m2 <- ulam(
  alist(
    A ~ bernoulli(p),
    logit(p) <- a[G,D],
    matrix[G,D]:a ~ normal(0,1)
  ), data=dat_sim, chains=4, cores=4
)

precis(m2,3)

# transformation of data from posterior distribution to get it on probability scale
inv_logit(coef(m2))
