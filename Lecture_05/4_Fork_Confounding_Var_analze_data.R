# ----------------------------------------------------------------#
##-                     Analyze the real data                   -##
# ----------------------------------------------------------------#

## ..................................................##
#                 *** Concept ***                 #
" 
  X <- Z -> Y 

"

#                 *** Formula ***                 #
" 
  Di ~ Normal(mu, sigma)
  mu = intercept + slope of people (marriage) + slope of people (age)
  mu = a + b_m * M_i + b_a * A-i
  a ~ Normal(0,10)
  b_m ~ Normal(0,10)
  b_a ~ Normal(0,10)
  sigma ~ Exponential(1)
"
library(rethinking)
data("WaffleDivorce")

d <- WaffleDivorce

dat <- list(
  D = standardize(d$Divorce),
  M = standardize(d$Marriage),
  A = standardize(d$MedianAgeMarriage)
)


m_DMA <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM*M + bA*A,
    a ~ dnorm(0,0.2),
    bM ~ dnorm(0,0.5),
    bA ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ), data=dat
)


plot(precis(m_DMA))


# ----------------------------------------------------------------#
##-                     Simulating Intervention                  -##
# ----------------------------------------------------------------#

post <- extract.samples((m_DMA))

# Sample A from data
n <- 1e3
As <- sample(dat$A, size=n, replace = TRUE)

# Simulate D for M=0 (sample mean)
DM0 <- with(post, 
            rnorm(n, a+bM*0 + bA*As, sigma))

# Simulate D for M=1 (+1 standard deviation)
# use the *same* A values
DM1 <- with(post, 
            rnorm(n, a+bM*1 + bA*As, sigma))

# contrast
M10_contrast <- DM1 - DM0
dens(M10_contrast, lwd=4, col=2, xlab='Effect of 1sd increase in M')
