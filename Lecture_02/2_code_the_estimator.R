# ---------------------------------------------------#
##- Design Bayesian Estimator - Code the estimator -##
# ---------------------------------------------------#

# Formula used:
# Ways for p to produce W,L = (4p)^W x (4-4p)^L

# import rethinking library to use make_bar() functionality

library(rethinking)
library(ggplot2)


posterior_prob <- function(sample){
  #sample <- c('W','L','W','W','W','L','W','L','W')
  W <- sum(sample == 'W')
  L <- sum(sample == 'L')
  poss <- c(0,0.25,0.50,0.75,1)
  ways <- sapply(poss, function(q) (q*4)^W * (4*(1-q))^L)
  prob <- ways/sum(ways)
  
  #cbind(poss, ways, round(prob, 2))
  
  graph <- sapply(poss, function(q) make_bar(q))
  
  # print(data.frame(poss, ways, round(prob, 2), graph))
  df = data.frame(poss, prob)
  
  ggplot(df, aes(x=poss, y=prob)) +
           geom_bar(stat='identity', fill='blue') +
           geom_point(color='red', size=3) +
           labs(title='Posterior Probability Distribution',
                x='Possible vlaues of p',
                y='Probability') +
    scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1)) +
            theme_minimal()
}


sample <- c('W','L','W','W','W','L','W','L','W')
posterior_prob(sample)


