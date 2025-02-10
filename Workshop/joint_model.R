N <- 9
p <- runif(1e4, 0, 1)
W <- rbinom(1e4, size=N, prob = p)
table(W)
