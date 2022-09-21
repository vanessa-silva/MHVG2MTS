#################################################################
##
## Function to simulate N bivariate GARCH process. (using CCC-GARCH algorithm)
##
##      t      : length of time serie
##      omega  : a vector of constants in the GARCH equation
##      alpha1 : an ARCH parameter matrix in the GARCH equation
##      beta1  : a GARCH parameter matrix in the GARCH equation
##      varcov : a constant conditional correlation matrix
##
#################################################################

## load library
if (!require(ccgarch))
  install.packages('ccgarch')


## garch simulation function definiton
cccGarch.simul <- function(t, omega, alpha, beta, varcov) {
  res <- eccc.sim(nobs = (t+200),
                  a = omega, A = alpha, B = beta, R = varcov,
                  d.f = 5, model = "diagonal")$eps

  return(res[201:(t+200),])
}
