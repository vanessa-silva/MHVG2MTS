#################################################################
#############  Simulate Bivariate Time Series Models ############
#################################################################


## load simmulation functions 
source("aux_code/sim_wn.R")
source("aux_code/sim_var.R")
source("aux_code/sim_garch.R")

## set seed
set.seed(032022)


## set parameters
m <- 2             # number of time series componnets/variables
T <- 10^4          # time series length to reps tests
n_inst <- 100      # total of bivariate time series

### covariace matrices
varcov_weak <- cbind(c(1, 0.1), c(0.1, 1.5))         # weak cross correlation to weak GARCH 
varcov_strong <- cbind(c(1, 0.86), c(0.86, 1.5))     # strong cross correlation to strong GARCH

### autoregressito vectors
phi_weak <- cbind(c(0.2, 0.02), c(0.1, 0.1))         # weak auto correlation to weak VAR
phi_strong <- cbind(c(0.7, 0.02), c(0.3, 0.8))       # strong auto correlation to strong VAR

### other matrices to GARCH models
alpha <- matrix(c(0.1, 0, 0, 0.05), ncol = 2)        # to GARCH model
beta <- matrix(c(0.85, 0, 0, 0.88), ncol = 2)        # to GARCH model

### other constants (vectors)
const_weak <- c(2.5, 0.5)                            # intercept terms to weak VAR
const_strong <- c(0, 0)                              # intercept terms to strong VAR
gamma <- 0.7					     # initial parameter value to GARCH models
omega <- c(0.05, 0.02)



## Generate DGP's


################################################
### simmulate White Noise Processes 
ind_bwn <- list()         ## independent WN
corr_bwn <- list()        ## correlated WN

for(i in 1:n_inst) {
  ind_bwn[[i]] <- wn.simul(T)
  corr_bwn[[i]] <- wn.simul(T, varcov_strong)
}

save(ind_bwn, corr_bwn, 
     file = "data/bwn_models.RData")


################################################
### simmulate VAR(1)
weak_var <- list()        ## weak VAR(1)
strong_var <- list()      ## satrong VAR(1)

for(i in 1:n_inst) {
  weak_var[[i]] <- var.simul(T, const_weak, phi_weak, varcov_weak)
  strong_var[[i]] <- var.simul(T, const_strong, phi_strong, varcov_strong)
}

save(weak_var, strong_var,
     file = "data/var_models.RData")


################################################
### simmulate GARCH(1,1) [CCC-GARCH(1,1)]
weak_garch <- list()      ## weak GARCH(1,1)
strong_garch <- list()    ## strong GARCH(1,1)

for(i in 1:n_inst) {
  weak_garch[[i]] <- cccGarch.simul(T, omega, alpha, beta, varcov_weak)
  strong_garch[[i]] <- cccGarch.simul(T, omega, alpha, beta, varcov_strong)
}

save(weak_garch, strong_garch,
     file = "data/garch_models.RData")
