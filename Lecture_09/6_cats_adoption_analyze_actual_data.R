#-----------------------------------------------------------------#
##-          Analyze cats adoption rate in Austin              -##
# ----------------------------------------------------------------#
"
"
library(rethinking)
data("AustinCats")

d <- AustinCats

dat <-list(
  days = d$days_to_event,
  adopted = ifelse(d$out_event=='Adoption',1,0),
  color_id = ifelse(d$color=='Black', 1,2)
)

meow <- ulam(
  alist(
    days|adopted == 1 ~ exponential(lambda),
    days|adopted == 0 ~ custom(exponential_lccdf(!Y|lambda)),
    lambda <- 1.0/mu,
    log(mu) <- a[color_id],
    a[color_id] ~ normal(0,1)
  ), data=dat, chains=4, cores=4  
)

