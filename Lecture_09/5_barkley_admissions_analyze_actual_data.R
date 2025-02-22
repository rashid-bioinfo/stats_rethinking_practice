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
