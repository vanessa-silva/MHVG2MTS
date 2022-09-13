#########################################################
##########          READ BTS DATA SET          ##########
#########################################################

### load DPG info's
source("libraries.R")
source("info_data.R")
source("aux_code/plot_functions.R")


### read csv files of experimental bivariate time series (DGP models)
bts_list <- list()
for(i in 1:length(model_names)) {
  bts_list[[i]]<- read.csv(paste0("data/sim_",
                                  model_names[i],
                                  ".csv"),
                           header = TRUE, sep = ",", dec = ".")
  colnames(bts_list[[i]]) <- c("Time", "Y_1", "Y_2")
}
names(bts_list) <- model_names
save(bts_list, file = "results/bts.RData")



## analysisBTS's
min <- 1000
max <- min+300
brk <- 50
plot_ibwn <- mts_plot(bts_list$iBWN[min:max, ], 
                      min, max, brk,
                      colors_bts$iBWN,
                      "Independent WN", "", "MTS")
plot_cbwn <- mts_plot(bts_list$cBWN[min:max, ], 
                      min, max, brk,
                      colors_bts$cBWN,
                      "Correlated WN", "", "")
plot_wvar <- mts_plot(bts_list$wVAR[min:max, ], 
                      min, max, brk,
                      colors_bts$wVAR,
                      "Weak VAR", "", "")
plot_svar <- mts_plot(bts_list$sVAR[min:max, ], 
                      min, max, brk,
                      colors_bts$sVAR,
                      "Strong VAR", "", "")
plot_wgarch <- mts_plot(bts_list$wGARCH[min:max, ], 
                        min, max, brk,
                        colors_bts$wGARCH,
                        "Weak GARCH", "", "")
plot_sgarch <- mts_plot(bts_list$sGARCH[min:max, ], 
                        min, max, brk,
                        colors_bts$sGARCH,
                        "Strong GARCH", "Time", "")
multiplot(plot_ibwn, plot_cbwn, 
          plot_wvar, plot_svar,
          plot_wgarch, plot_sgarch,
          cols = 1)
save(plot_ibwn, plot_cbwn, 
     plot_wvar, plot_svar,
     plot_wgarch, plot_sgarch, 
     file = "results/bts_plots.RData")



## analysis ACF's
range <- c(1000:1500)
linf <- -0.2
lsup <- 1
acfPlot_ibwn <- acf_plot(bts_list$iBWN$Y_1[range],
                         "", "", "Independent WN",  
                         linf, lsup)
acfPlot_ibwn2 <- acf_plot(bts_list$iBWN$Y_2[range],
                          "", "", "",  
                          linf, lsup)
acfPlot_cbwn <- acf_plot(bts_list$cBWN$Y_1[range],
                         "", "", "Correlated WN",  
                         linf, lsup)
acfPlot_cbwn2 <- acf_plot(bts_list$cBWN$Y_2[range],
                          "", "", "",  
                          linf, lsup)
acfPlot_wvar <- acf_plot(bts_list$wVAR$Y_1[range],  
                         "", "", "Weak VAR",  
                         linf, lsup)
acfPlot_wvar2 <- acf_plot(bts_list$wVAR$Y_2[range],
                          "", "", "",  
                          linf, lsup)
acfPlot_svar <- acf_plot(bts_list$sVAR$Y_1[range],
                         "", "", "Strong VAR",  
                         linf, lsup)
acfPlot_svar2 <- acf_plot(bts_list$sVAR$Y_2[range],
                          "", "", "",  
                          linf, lsup)
acfPlot_wgarch <- acf_plot(bts_list$wGARCH$Y_1[range],
                           "", "", "Weak GARCH",  
                           linf, lsup)
acfPlot_wgarch2 <- acf_plot(bts_list$wGARCH$Y_2[range], 
                            "", "", "",  
                            linf, lsup)
acfPlot_sgarch <- acf_plot(bts_list$sGARCH$Y_1[range],
                           "", "Lag", "Strong GARCH",  
                           linf, lsup)
acfPlot_sgarch2 <- acf_plot(bts_list$sGARCH$Y_2[range],
                            "", "Lag", "",  
                            linf, lsup)
multiplot(acfPlot_ibwn, acfPlot_cbwn, 
          acfPlot_wvar, acfPlot_svar, 
          acfPlot_wgarch, acfPlot_sgarch, 
          acfPlot_ibwn2, acfPlot_cbwn2,
          acfPlot_wvar2, acfPlot_svar2, 
          acfPlot_wgarch2, acfPlot_sgarch2,   
          cols = 2)
save(acfPlot_ibwn, acfPlot_ibwn2, 
     acfPlot_cbwn, acfPlot_cbwn2, 
     acfPlot_wvar, acfPlot_wvar2, 
     acfPlot_svar, acfPlot_svar2,
     acfPlot_wgarch, acfPlot_wgarch2, 
     acfPlot_sgarch, acfPlot_sgarch2, 
     file = "results/acf_plots.RData")



## analysis CCF's
ccfPlot_ibwn <- ccf_plot(bts_list$iBWN$Y_1[range], bts_list$iBWN$Y_2[range],
                         "", "", "CCF",
                         linf, lsup)
ccfPlot_cbwn <- ccf_plot(bts_list$cBWN$Y_1[range], bts_list$cBWN$Y_2[range],
                         "", "", "",
                         linf, lsup)
ccfPlot_wvar <- ccf_plot(bts_list$wVAR$Y_1[range], bts_list$wVAR$Y_2[range],
                         "", "", "",
                         linf, lsup)
ccfPlot_svar <- ccf_plot(bts_list$sVAR$Y_1[range], bts_list$sVAR$Y_2[range],
                         "", "", "",
                         linf, lsup)
ccfPlot_wgarch <- ccf_plot(bts_list$wGARCH$Y_1[range], bts_list$wGARCH$Y_2[range],
                           "", "", "",
                           linf, lsup)
ccfPlot_sgarch <- ccf_plot(bts_list$sGARCH$Y_1[range], bts_list$sGARCH$Y_2[range],
                           "", "Lag", "",
                           linf, lsup,
                           flag = 1)
multiplot(ccfPlot_ibwn, ccfPlot_cbwn, 
          ccfPlot_wvar, ccfPlot_svar,
          ccfPlot_wgarch, ccfPlot_sgarch, 
          cols = 1)
save(ccfPlot_ibwn, ccfPlot_cbwn, 
     ccfPlot_wvar, ccfPlot_svar,
     ccfPlot_wgarch, ccfPlot_sgarch, 
     file = "results/ccf_plots.RData")


