#########################################################
#########      INTRA, INTER and ALL-layers      #########
########   Degree Sequences and Distributions    ########
#########################################################

### load libraries, auxiliary functions and DPG info's
source("libraries.R")
source("info_data.R")
source("aux_code/utils_functions.R")
source("aux_code/seqs_functions.R")
source("aux_code/plot_functions.R")


######################################################
### Compute Distribution's of Degree Sequences
load("results/DegreeSeq.RData")

### INTRA
dd_intra_Y1 <- dist_degree(LocalDegreeSeq, model_names, n_inst, 1)
dd_intra_Y2 <- dist_degree(LocalDegreeSeq, model_names, n_inst, 2)

## calculate mean of degree distributions by models
mean_dd_intra_Y1 <- mean_dist_degree(dd_intra_Y1, n_inst)
mean_dd_intra_Y2 <- mean_dist_degree(dd_intra_Y2, n_inst)
## calculate standard deviation of degree distributions by models
sd_dd_intra_Y1 <- sd_dist_degree(dd_intra_Y1, n_inst)
sd_dd_intra_Y2 <- sd_dist_degree(dd_intra_Y2, n_inst)
#### até k = 22


### INTER
dd_inter_Y1 <- dist_degree(LocalDegreeSeq, model_names, n_inst, 3)
dd_inter_Y2 <- dist_degree(LocalDegreeSeq, model_names, n_inst, 4)

## calculate mean of degree distributions by models
mean_dd_inter_Y1 <- mean_dist_degree(dd_inter_Y1, n_inst)
mean_dd_inter_Y2 <- mean_dist_degree(dd_inter_Y2, n_inst)
## calculate standard deviation of degree distributions by models
sd_dd_inter_Y1 <- sd_dist_degree(dd_inter_Y1, n_inst)
sd_dd_inter_Y2 <- sd_dist_degree(dd_inter_Y2, n_inst)
#### até k = 14


### ALL
dd_all_Y1 <- dist_degree(LocalDegreeSeq, model_names, n_inst, 5)
dd_all_Y2 <- dist_degree(LocalDegreeSeq, model_names, n_inst, 6)

## calculate mean of degree distributions by models
mean_dd_all_Y1 <- mean_dist_degree(dd_all_Y1, n_inst)
mean_dd_all_Y1 <- mean_dd_all_Y1[-c(1,2), ]
mean_dd_all_Y2 <- mean_dist_degree(dd_all_Y2, n_inst)
mean_dd_all_Y2 <- mean_dd_all_Y2[-c(1,2), ]
## calculate standard deviation of degree distributions by models
sd_dd_all_Y1 <- sd_dist_degree(dd_all_Y1, n_inst)
sd_dd_all_Y1 <- sd_dd_all_Y1[-c(1,2), ]
sd_dd_all_Y2 <- sd_dist_degree(dd_all_Y2, n_inst)
sd_dd_all_Y2 <- sd_dd_all_Y2[-c(1,2), ]
#### até k = 32


### save degree distributions
save(dd_intra_Y1, dd_intra_Y2,
     mean_dd_intra_Y1, mean_dd_intra_Y2,
     sd_dd_intra_Y1, sd_dd_intra_Y2,
     dd_inter_Y1, dd_inter_Y2,
     mean_dd_inter_Y1, mean_dd_inter_Y2,
     sd_dd_inter_Y1, sd_dd_inter_Y2,
     dd_all_Y1, dd_all_Y2,
     mean_dd_all_Y1, mean_dd_all_Y2,
     sd_dd_all_Y1, sd_dd_all_Y2,
     file = "results/DegreeDistribution.RData")


######################################################
### Plot Degree Distribution's
load("results/DegreeDistribution.RData")
smooth <- 0
semilog <- 0
log <- 1
k_brk <- 4

scale_type <- "scalefree"
if(semilog)
  scale_type <- "semilogscale"
if(log)
  scale_type <- "logscale"
xtitle <- "k"
if(log)
  xtitle <- "log(k)"

#########
### intra degree distributions mean
k_min <- 2
k_max <- 22-1
linf <- 0.0001#-1#0.0001#0
lsup <- 1#-1#1#0.35

## WN
ddIntraPlot_ibwn <- melt_variables(mean_dd_intra_Y1[1:k_max, c('k', 'iBWN')],
                                   mean_dd_intra_Y2[1:k_max, c('k', 'iBWN')]) %>%
  dist_plot(k_min, k_max+1, k_brk, linf = linf, lsup = lsup,
            colors_bts$iBWN,
            "", "", "P(k): Intra-Layer",
            is_smooth = smooth, is_semilog = semilog, is_log = log) +
  theme(legend.position = "none")
ddIntraPlot_cbwn <- melt_variables(mean_dd_intra_Y1[1:k_max, c('k', 'cBWN')],
                                   mean_dd_intra_Y2[1:k_max, c('k', 'cBWN')]) %>%
  dist_plot(k_min, k_max+1, k_brk, linf = linf, lsup = lsup, 
            colors_bts$cBWN,
            "", "", "",
            is_smooth = smooth, is_semilog = semilog, is_log = log) +
  theme(legend.position = "none")
## VAR
ddIntraPlot_wvar <- melt_variables(mean_dd_intra_Y1[1:k_max, c('k', 'wVAR')],
                                   mean_dd_intra_Y2[1:k_max, c('k', 'wVAR')]) %>%
  dist_plot(k_min, k_max+1, k_brk, linf = linf, lsup = lsup, 
            colors_bts$wVAR,
            "", "", "",
            is_smooth = smooth, is_semilog = semilog, is_log = log) +
  theme(legend.position = "none")
ddIntraPlot_svar <- melt_variables(mean_dd_intra_Y1[1:k_max, c('k', 'sVAR')],
                                   mean_dd_intra_Y2[1:k_max, c('k', 'sVAR')]) %>%
  dist_plot(k_min, k_max+1, k_brk, linf = linf, lsup = lsup, 
            colors_bts$sVAR, 
            "", "", "",
            is_smooth = smooth, is_semilog = semilog, is_log = log) +
  theme(legend.position = "none")
## GARCH
ddIntraPlot_wgarch <- melt_variables(mean_dd_intra_Y1[1:k_max, c('k', 'wGARCH')],
                                     mean_dd_intra_Y2[1:k_max, c('k', 'wGARCH')]) %>%
  dist_plot(k_min, k_max+1, k_brk, linf = linf, lsup = lsup, 
            colors_bts$wGARCH,
            "", "", "",
            is_smooth = smooth, is_semilog = semilog, is_log = log) +
  theme(legend.position = "none")
ddIntraPlot_sgarch <- melt_variables(mean_dd_intra_Y1[1:k_max, c('k', 'sGARCH')],
                                     mean_dd_intra_Y2[1:k_max, c('k', 'sGARCH')]) %>%
  dist_plot(k_min, k_max+1, k_brk, linf = linf, lsup = lsup, 
            colors_bts$sGARCH,
            "", xtitle, "",
            is_smooth = smooth, is_semilog = semilog, is_log = log,
            flag = 1) +
  theme(legend.position = "none")

multiplot(ddIntraPlot_ibwn, ddIntraPlot_cbwn,
          ddIntraPlot_wvar, ddIntraPlot_svar,
          ddIntraPlot_wgarch, ddIntraPlot_sgarch,
          cols = 1)
save(ddIntraPlot_ibwn, ddIntraPlot_cbwn,
     ddIntraPlot_wvar, ddIntraPlot_svar,
     ddIntraPlot_wgarch, ddIntraPlot_sgarch,
     file = paste0("results/ddIntra_", scale_type,"_plots.RData"))

#########
### inter degree distributions mean
k_min <- 2
k_max <- 14-1
linf <- 0.0001#-1#0#0.0001
lsup <- 1#-1#1#1

## WN
ddInterPlot_ibwn <- melt_variables(mean_dd_inter_Y1[1:k_max, c('k', 'iBWN')],
                                   mean_dd_inter_Y2[1:k_max, c('k', 'iBWN')]) %>%
  dist_plot(k_min, k_max+1, k_brk, linf = linf, lsup = lsup,
            colors_bts$iBWN,
            "", "", "P(k): Inter-Layer",
            is_smooth = smooth, is_semilog = semilog, is_log = log) +
  theme(legend.position = "none")
ddInterPlot_cbwn <- melt_variables(mean_dd_inter_Y1[1:k_max, c('k', 'cBWN')],
                                   mean_dd_inter_Y2[1:k_max, c('k', 'cBWN')]) %>%
  dist_plot(k_min, k_max+1, k_brk, linf = linf, lsup = lsup, 
            colors_bts$cBWN,
            "", "", "",
            is_smooth = smooth, is_semilog = semilog, is_log = log) +
  theme(legend.position = "none")
## VAR
ddInterPlot_wvar <- melt_variables(mean_dd_inter_Y1[1:k_max, c('k', 'wVAR')],
                                   mean_dd_inter_Y2[1:k_max, c('k', 'wVAR')]) %>%
  dist_plot(k_min, k_max+1, k_brk, linf = linf, lsup = lsup, 
            colors_bts$wVAR,
            "", "", "",
            is_smooth = smooth, is_semilog = semilog, is_log = log) +
  theme(legend.position = "none")
ddInterPlot_svar <- melt_variables(mean_dd_inter_Y1[1:k_max, c('k', 'sVAR')],
                                   mean_dd_inter_Y2[1:k_max, c('k', 'sVAR')]) %>%
  dist_plot(k_min, k_max+1, k_brk, linf = linf, lsup = lsup, 
            colors_bts$sVAR, 
            "", "", "",
            is_smooth = smooth, is_semilog = semilog, is_log = log) +
  theme(legend.position = "none")
## GARCH
ddInterPlot_wgarch <- melt_variables(mean_dd_inter_Y1[1:k_max, c('k', 'wGARCH')],
                                     mean_dd_inter_Y2[1:k_max, c('k', 'wGARCH')]) %>%
  dist_plot(k_min, k_max+1, k_brk, linf = linf, lsup = lsup, 
            colors_bts$wGARCH,
            "", "", "",
            is_smooth = smooth, is_semilog = semilog, is_log = log) +
  theme(legend.position = "none")
ddInterPlot_sgarch <- melt_variables(mean_dd_inter_Y1[1:k_max, c('k', 'sGARCH')],
                                     mean_dd_inter_Y2[1:k_max, c('k', 'sGARCH')]) %>%
  dist_plot(k_min, k_max+1, k_brk, linf = linf, lsup = lsup, 
            colors_bts$sGARCH,
            "", xtitle, "",
            is_smooth = smooth, is_semilog = semilog, is_log = log,
            flag = 1) +
  theme(legend.position = "none")

multiplot(ddInterPlot_ibwn, ddInterPlot_cbwn,
          ddInterPlot_wvar, ddInterPlot_svar,
          ddInterPlot_wgarch, ddInterPlot_sgarch,
          cols = 1)
save(ddInterPlot_ibwn, ddInterPlot_cbwn,
     ddInterPlot_wvar, ddInterPlot_svar,
     ddInterPlot_wgarch, ddInterPlot_sgarch,
     file = paste0("results/ddInter_", scale_type, "_plots.RData"))

#########
### all degree distributions mean
k_min <- 4
k_max <- 32-3
linf <- 0.0001#-1#0#0.0001
lsup <- 1#-1#0.35#1

## WN
ddAllPlot_ibwn <- melt_variables(mean_dd_all_Y1[1:k_max, c('k', 'iBWN')],
                                 mean_dd_all_Y2[1:k_max, c('k', 'iBWN')]) %>%
  dist_plot(k_min, k_max+3, k_brk, linf = linf, lsup = lsup,
            colors_bts$iBWN,
            "", "", "P(k): All-Layer",
            is_smooth = smooth, is_semilog = semilog, is_log = log) +
  theme(legend.position = "none")
ddAllPlot_cbwn <- melt_variables(mean_dd_all_Y1[1:k_max, c('k', 'cBWN')],
                                 mean_dd_all_Y2[1:k_max, c('k', 'cBWN')]) %>%
  dist_plot(k_min, k_max+3, k_brk, linf = linf, lsup = lsup, 
            colors_bts$cBWN,
            "", "", "",
            is_smooth = smooth, is_semilog = semilog, is_log = log) +
  theme(legend.position = "none")
## VAR
ddAllPlot_wvar <- melt_variables(mean_dd_all_Y1[1:k_max, c('k', 'wVAR')],
                                 mean_dd_all_Y2[1:k_max, c('k', 'wVAR')]) %>%
  dist_plot(k_min, k_max+3, k_brk, linf = linf, lsup = lsup, 
            colors_bts$wVAR,
            "", "", "",
            is_smooth = smooth, is_semilog = semilog, is_log = log) +
  theme(legend.position = "none")
ddAllPlot_svar <- melt_variables(mean_dd_all_Y1[1:k_max, c('k', 'sVAR')],
                                 mean_dd_all_Y2[1:k_max, c('k', 'sVAR')]) %>%
  dist_plot(k_min, k_max+3, k_brk, linf = linf, lsup = lsup, 
            colors_bts$sVAR, 
            "", "", "",
            is_smooth = smooth, is_semilog = semilog, is_log = log) +
  theme(legend.position = "none")
## GARCH
ddAllPlot_wgarch <- melt_variables(mean_dd_all_Y1[1:k_max, c('k', 'wGARCH')],
                                   mean_dd_all_Y2[1:k_max, c('k', 'wGARCH')]) %>%
  dist_plot(k_min, k_max+3, k_brk, linf = linf, lsup = lsup, 
            colors_bts$wGARCH,
            "", "", "",
            is_smooth = smooth, is_semilog = semilog, is_log = log) +
  theme(legend.position = "none")
ddAllPlot_sgarch <- melt_variables(mean_dd_all_Y1[1:k_max, c('k', 'sGARCH')],
                                   mean_dd_all_Y2[1:k_max, c('k', 'sGARCH')]) %>%
  dist_plot(k_min, k_max+3, k_brk, linf = linf, lsup = lsup, 
            colors_bts$sGARCH,
            "", xtitle, "",
            is_smooth = smooth, is_semilog = semilog, is_log = log,
            flag = 1) +
  theme(legend.position = "none")

multiplot(ddAllPlot_ibwn, ddAllPlot_cbwn,
          ddAllPlot_wvar, ddAllPlot_svar,
          ddAllPlot_wgarch, ddAllPlot_sgarch,
          cols = 1)
save(ddAllPlot_ibwn, ddAllPlot_cbwn,
     ddAllPlot_wvar, ddAllPlot_svar,
     ddAllPlot_wgarch, ddAllPlot_sgarch,
     file = paste0("results/ddAll_", scale_type, "_plots.RData"))

