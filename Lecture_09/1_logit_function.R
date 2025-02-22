#-----------------------------------------------------------------#
##-                 logit function - Brenouli Model              -##
# ----------------------------------------------------------------#
"
logit(pi) = alpha + beta(xi)

alpha a ~ Normal(0,10)
beta b ~ Normal(0,10)
"

library(rethinking)
library(boot)

# SD = 10
#a <- rnorm(1e4, 0, 10)
#b <- rnorm(1e4, 0, 10)

# SD = 1.5
a <- rnorm(1e4, 0, 1.5)
b <- rnorm(1e4, 0, 0.5)

xseq <- seq(from=-3,to=3, len=100)

p <- sapply(xseq, function(x) inv_logit(a+b+x))

plot(NULL, xlim=c(-2.5,2.5), ylim=c(0,1),
     xlab="x value", ylab="probability")
for (i in 1:10) lines(xseq, p[i, ], lwd=3, col=2)
