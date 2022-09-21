#################################################################
##
## Function to simulate bivariate VAR process
##      
##      T       : length of output time series
##      const   : vector of intercept terms
##      phi     : matrix of AR coefficients 
##      cov.mat : covariance matrix 
##
#################################################################

## load library
if (!require(mAr))
  install.packages('mAr')


## var simulation function definiton
var.simul <- function(T, const, phi, cov.mat) {
  burn <- 500
  res <- mAr.sim(w = const, A = phi, C = cov.mat, N = (T + burn))
  
  return(as.matrix(res[(burn + 1):(T + burn), ]))
}
