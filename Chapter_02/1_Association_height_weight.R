# ----------------------------------------------------------------#
##-        Calculate association of height and weight           -##
# ----------------------------------------------------------------#

## Formula used:
# W = bH + U
# W: weight, bH: proportion of height
# U: Unobserved data (amount of noise)

sim_weight <- function(H,b, sd){
  # rnorm(height, mean, standard deviation)
  U <- rnorm(length(H), 0, sd)
  W <- (b*H) + U
  return(W)
}

H <- runif(200, min=130, max=170)
# df <- data.frame(height=H)
# df
W <- sim_weight(H, b=0.5, sd=5)
df1 <- data.frame(height=H)
df1
plot(W ~ H, col=4, lwd=2)
