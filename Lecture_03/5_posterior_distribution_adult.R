# ----------------------------------------------------------------#
##-    Prior Predictive Simulation - Simulation of 10 people    -##
# ----------------------------------------------------------------#

## ..................................................##
#                 *** Formulation ***                 #
" 
  Coding notations: 
  a = alpha
  b = beta
  o = sigma

  Wi ~ Normal (mu, sigma)
  mu = a + bHi
  a ~ Normal(0,10)
  b ~ Uniform(0,1)
  o ~ Uniform(0,10)
"

# Run the model
library(rethinking)
data("Howell1")

# Select the data of adults only
d2 <- Howell1[Howell1$age>18,]

dat <- list(W=d2$weight, H=d2$height)

adults <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + (b*H),
    a ~ dnorm(0,10),
    b ~ dunif(0,1),
    sigma ~ dunif(0,10)
  ),data=dat
)

# summary
precis(adults)


post <- extract.samples(adults)
head(post)

# Making plot
plot(d2$height, d2$weight, col=4, lwd=3,
     xlab="Height (cm)", ylab="Weight (kg)")
for(j in 1:20)
  abline(a=post$a[j], b=post$b[j], lwd=2, col=2)
