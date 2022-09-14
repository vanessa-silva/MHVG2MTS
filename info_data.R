#####################################################
##### Auxiliary variables
#####################################################

## DGP models
model_names <- c("iBWN", "cBWN",                       ## WN models
                 "wVAR", "sVAR",                       ## VAR models
                 "wGARCH", "sGARCH")                   ## GARCH models


colors_bts <- list(c("green4", "limegreen"),           ## iBWN
                   c("darkorange3", "orange"),         ## cBWN
                   c("violetred3", "lightpink"),       ## wVAR
                   c("red3", "salmon"),                ## sVAR
                   c("cyan4", "cyan"),                 ## wGARCH
                   c("darkblue", "cornflowerblue"))    ## sGARCH
names(colors_bts) <- model_names

n_inst <- 100

layer_names <- c("V_1", "V_2", "V_2all")

