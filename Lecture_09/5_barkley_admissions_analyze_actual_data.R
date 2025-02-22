#-----------------------------------------------------------------#
##-           Checking gender biases in Berkely                -##
# ----------------------------------------------------------------#
"

G --> A
G --> D --> A

"

library(rethinking)
data("UCBadmit")

d <- UCBadmit

dat <- list(
  A = d$admit,
  N = d$applications,
  G = ifelse(d$applicant.gender=='female',1,2),
  D = as.integer(d$dept)
)

## - Total gender effect

mG <- ulam(
  alist(
    A ~ binomial(N,p),
    logit(p) <- a[G],
    a[G] ~ normal(0,1)
  ), data=dat, chains=4, cores=4
)

precis(mG,2)

## - Direct gender effect

mGD <- ulam(
  alist(
    A ~ binomial(N,p),
    logit(p) <- a[G,D],
    matrix[G,D]:a ~ normal(0,1)
  ), data=dat, chains=4, cores=4
)

precis(mGD,3)


# Extracting samples (Total effect)

post1 <- extract.samples(mG)
PrA_G1 <- inv_logit(post1$a[,1])
PrA_G2 <- inv_logit(post1$a[,2])
diff_prob <- PrA_G1 - PrA_G2
dens(diff_prob, lwd=4, col=2, xlab="Gender contrast (Probability)")

# Extracting samples (Direct effect)

post2 <- extract.samples(mGD)
PrA <- inv_logit(post2$a)
diff_prob_D_<- sapply(1:6, function(i)
  PrA[,1,i] - PrA[,2,i])

plot(NULL, xlim=c(-0.2,0.3), ylim=c(0,25), xlab="Gender contrast (Probability)", ylab="Density")
for(i in 1:6) dens(diff_prob_D_[,i], lwd=4, col=1+i, add=TRUE) 
