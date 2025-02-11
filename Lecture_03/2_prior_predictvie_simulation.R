# ----------------------------------------------------------------#
##-                     Prior Predictive Simulation              -##
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
n <- 1e3
b <- runif(n,0,1)
a <- rnorm(n,0,10)
b_df <- data.frame(b)
a_df <- data.frame(a)

plot(NULL, xlim=c(130,170), ylim=c(50,90),
     xlab='height (cm)', ylab='weight (kg)')
for (j in 1:50) abline(a=a[j], b=b[j], lwd=2, col=2)
