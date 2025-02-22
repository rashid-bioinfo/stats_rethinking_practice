#-----------------------------------------------------------------#
##-                       Perceived Gender                -##
# ----------------------------------------------------------------#
"

G --> G* --> A
G --> D --> A
"
# Importing libraries

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

#________________________________________________

# Number of applications to simulate
total_apps <- sum(dat$N)

# Number of applications per department
apps_per_dept <- sapply(1:6, function(i)
  sum(dat$N[dat$D==i]))

# Simulate as if all applicants are women
p_G1 <- link(mGD, data=list(
  D = rep(1:6, times=apps_per_dept),
  N = rep(1, total_apps),
  G = rep(1, total_apps)
))

# Simulate as if all applicants are men
p_G2 <- link(mGD, data=list(
  D = rep(1:6, times=apps_per_dept),
  N = rep(1, total_apps),
  G = rep(2, total_apps)
))

# Summarize
dens(p_G1 - p_G2, lwd=3, col=2, xlab="Effect of gender perception")


