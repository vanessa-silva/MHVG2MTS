#################################################################
##
## Function to simulate bivariate WHITE NOISE process
##      
##      T       : time serie length 
##      cov.mat : covariance matrix
##
#################################################################

## load library
library(mvtnorm)


## function definiton
wn.simul <- function(T, cov.mat = cbind(c(1, 0), c(0, 1))) {
  return(rmvnorm(T, sigma = cov.mat))
}
