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


table(G,D)
table(G,A)
