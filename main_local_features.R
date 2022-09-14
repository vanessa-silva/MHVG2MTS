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


######################################################
### BoxPlots of Degree Distribution set
load("results/DegreeDistribution.RData")
k_brk <- 4
xtitle <- "k"

#########
### INTRA
k_min <- 2
k_max <- 22
linf <- 0
lsup <- 0.35

df_dd_intra_Y1 <- df_dists(dd_intra_Y1, model_names, n_inst, k_min, k_max)
df_dd_intra_Y2 <- df_dists(dd_intra_Y2, model_names, n_inst, k_min, k_max)

## WN
df_dd_intra_ibwn <- rbind(cbind(freq_df(df_dd_intra_Y1$iBWN), "Variable" = "Y_1"),
                          cbind(freq_df(df_dd_intra_Y2$iBWN), "Variable" = "Y_2"))
ddIntraBoxPlot_ibwn <- boxplot_dists(df_dd_intra_ibwn,
                                     k_min, k_max, k_brk, linf = linf, lsup = lsup,
                                     colors_bts$iBWN,
                                     "", "", "P(k): Intra-Layer")
df_dd_intra_cbwn <- rbind(cbind(freq_df(df_dd_intra_Y1$cBWN), "Variable" = "Y_1"),
                          cbind(freq_df(df_dd_intra_Y2$cBWN), "Variable" = "Y_2"))
ddIntraBoxPlot_cbwn <- boxplot_dists(df_dd_intra_cbwn,
                                     k_min, k_max, k_brk, linf = linf, lsup = lsup,
                                     colors_bts$cBWN,
                                     "", "", "")
## VAR
df_dd_intra_wvar <- rbind(cbind(freq_df(df_dd_intra_Y1$wVAR), "Variable" = "Y_1"),
                          cbind(freq_df(df_dd_intra_Y2$wVAR), "Variable" = "Y_2"))
ddIntraBoxPlot_wvar <- boxplot_dists(df_dd_intra_wvar,
                                     k_min, k_max, k_brk, linf = linf, lsup = lsup,
                                     colors_bts$wVAR,
                                     "", "", "")
df_dd_intra_svar <- rbind(cbind(freq_df(df_dd_intra_Y1$sVAR), "Variable" = "Y_1"),
                          cbind(freq_df(df_dd_intra_Y2$sVAR), "Variable" = "Y_2"))
ddIntraBoxPlot_svar <- boxplot_dists(df_dd_intra_svar,
                                     k_min, k_max, k_brk, linf = linf, lsup = lsup,
                                     colors_bts$sVAR,
                                     "", "", "")
## GARCH
df_dd_intra_wgarch <- rbind(cbind(freq_df(df_dd_intra_Y1$wGARCH), "Variable" = "Y_1"),
                            cbind(freq_df(df_dd_intra_Y2$wGARCH), "Variable" = "Y_2"))
ddIntraBoxPlot_wgarch <- boxplot_dists(df_dd_intra_wgarch,
                                       k_min, k_max, k_brk, linf = linf, lsup = lsup,
                                       colors_bts$wGARCH,
                                       "", "", "")
df_dd_intra_sgarch <- rbind(cbind(freq_df(df_dd_intra_Y1$sGARCH), "Variable" = "Y_1"),
                            cbind(freq_df(df_dd_intra_Y2$sGARCH), "Variable" = "Y_2"))
ddIntraBoxPlot_sgarch <- boxplot_dists(df_dd_intra_sgarch,
                                       k_min, k_max, k_brk, linf = linf, lsup = lsup,
                                       colors_bts$sGARCH,
                                       "", xtitle, "",
                                       flag = 0)

multiplot(ddIntraBoxPlot_ibwn, ddIntraBoxPlot_cbwn,
          ddIntraBoxPlot_wvar, ddIntraBoxPlot_svar,
          ddIntraBoxPlot_wgarch, ddIntraBoxPlot_sgarch,
          cols = 6)
save(ddIntraBoxPlot_ibwn, ddIntraBoxPlot_cbwn,
     ddIntraBoxPlot_wvar, ddIntraBoxPlot_svar,
     ddIntraBoxPlot_wgarch, ddIntraBoxPlot_sgarch,
     file = paste0("results/ddIntraBoxPlot.RData"))


#########
### INTER
k_min <- 2
k_max <- 14
linf <- 0
lsup <- 1

df_dd_inter_Y1 <- df_dists(dd_inter_Y1, model_names, n_inst, k_min, k_max)
df_dd_inter_Y2 <- df_dists(dd_inter_Y2, model_names, n_inst, k_min, k_max)

## WN
df_dd_inter_ibwn <- rbind(cbind(freq_df(df_dd_inter_Y1$iBWN), "Variable" = "Y_1"),
                          cbind(freq_df(df_dd_inter_Y2$iBWN), "Variable" = "Y_2"))
ddInterBoxPlot_ibwn <- boxplot_dists(df_dd_inter_ibwn,
                                     k_min, k_max, k_brk, linf = linf, lsup = lsup,
                                     colors_bts$iBWN,
                                     "", "", "P(k): Inter-Layer")
df_dd_inter_cbwn <- rbind(cbind(freq_df(df_dd_inter_Y1$cBWN), "Variable" = "Y_1"),
                          cbind(freq_df(df_dd_inter_Y2$cBWN), "Variable" = "Y_2"))
ddInterBoxPlot_cbwn <- boxplot_dists(df_dd_inter_cbwn,
                                     k_min, k_max, k_brk, linf = linf, lsup = lsup,
                                     colors_bts$cBWN,
                                     "", "", "")
## VAR
df_dd_inter_wvar <- rbind(cbind(freq_df(df_dd_inter_Y1$wVAR), "Variable" = "Y_1"),
                          cbind(freq_df(df_dd_inter_Y2$wVAR), "Variable" = "Y_2"))
ddInterBoxPlot_wvar <- boxplot_dists(df_dd_inter_wvar,
                                     k_min, k_max, k_brk, linf = linf, lsup = lsup,
                                     colors_bts$wVAR,
                                     "", "", "")
df_dd_inter_svar <- rbind(cbind(freq_df(df_dd_inter_Y1$sVAR), "Variable" = "Y_1"),
                          cbind(freq_df(df_dd_inter_Y2$sVAR), "Variable" = "Y_2"))
ddInterBoxPlot_svar <- boxplot_dists(df_dd_inter_svar,
                                     k_min, k_max, k_brk, linf = linf, lsup = lsup,
                                     colors_bts$sVAR,
                                     "", "", "")
## GARCH
df_dd_inter_wgarch <- rbind(cbind(freq_df(df_dd_inter_Y1$wGARCH), "Variable" = "Y_1"),
                            cbind(freq_df(df_dd_inter_Y2$wGARCH), "Variable" = "Y_2"))
ddInterBoxPlot_wgarch <- boxplot_dists(df_dd_inter_wgarch,
                                       k_min, k_max, k_brk, linf = linf, lsup = lsup,
                                       colors_bts$wGARCH,
                                       "", "", "")
df_dd_inter_sgarch <- rbind(cbind(freq_df(df_dd_inter_Y1$sGARCH), "Variable" = "Y_1"),
                            cbind(freq_df(df_dd_inter_Y2$sGARCH), "Variable" = "Y_2"))
ddInterBoxPlot_sgarch <- boxplot_dists(df_dd_inter_sgarch,
                                       k_min, k_max, k_brk, linf = linf, lsup = lsup,
                                       colors_bts$sGARCH,
                                       "", xtitle, "",
                                       flag = 0)

multiplot(ddInterBoxPlot_ibwn, ddInterBoxPlot_cbwn,
          ddInterBoxPlot_wvar, ddInterBoxPlot_svar,
          ddInterBoxPlot_wgarch, ddInterBoxPlot_sgarch,
          cols = 6)
save(ddInterBoxPlot_ibwn, ddInterBoxPlot_cbwn,
     ddInterBoxPlot_wvar, ddInterBoxPlot_svar,
     ddInterBoxPlot_wgarch, ddInterBoxPlot_sgarch,
     file = paste0("results/ddInterBoxPlot.RData"))


#########
### ALL
k_min <- 4
k_max <- 32
linf <- 0
lsup <- 0.35

df_dd_all_Y1 <- df_dists(dd_all_Y1, model_names, n_inst, k_min, k_max)
df_dd_all_Y2 <- df_dists(dd_all_Y2, model_names, n_inst, k_min, k_max)

## WN
df_dd_all_ibwn <- rbind(cbind(freq_df(df_dd_all_Y1$iBWN), "Variable" = "Y_1"),
                        cbind(freq_df(df_dd_all_Y2$iBWN), "Variable" = "Y_2"))
ddAllBoxPlot_ibwn <- boxplot_dists(df_dd_all_ibwn,
                                   k_min, k_max, k_brk, linf = linf, lsup = lsup,
                                   colors_bts$iBWN,
                                   "", "", "P(k): All-Layer")
df_dd_all_cbwn <- rbind(cbind(freq_df(df_dd_all_Y1$cBWN), "Variable" = "Y_1"),
                        cbind(freq_df(df_dd_all_Y2$cBWN), "Variable" = "Y_2"))
ddAllBoxPlot_cbwn <- boxplot_dists(df_dd_all_cbwn,
                                   k_min, k_max, k_brk, linf = linf, lsup = lsup,
                                   colors_bts$cBWN,
                                   "", "", "")
## VAR
df_dd_all_wvar <- rbind(cbind(freq_df(df_dd_all_Y1$wVAR), "Variable" = "Y_1"),
                        cbind(freq_df(df_dd_all_Y2$wVAR), "Variable" = "Y_2"))
ddAllBoxPlot_wvar <- boxplot_dists(df_dd_all_wvar,
                                   k_min, k_max, k_brk, linf = linf, lsup = lsup,
                                   colors_bts$wVAR,
                                   "", "", "")
df_dd_all_svar <- rbind(cbind(freq_df(df_dd_all_Y1$sVAR), "Variable" = "Y_1"),
                        cbind(freq_df(df_dd_all_Y2$sVAR), "Variable" = "Y_2"))
ddAllBoxPlot_svar <- boxplot_dists(df_dd_all_svar,
                                   k_min, k_max, k_brk, linf = linf, lsup = lsup,
                                   colors_bts$sVAR,
                                   "", "", "")
## GARCH
df_dd_all_wgarch <- rbind(cbind(freq_df(df_dd_all_Y1$wGARCH), "Variable" = "Y_1"),
                          cbind(freq_df(df_dd_all_Y2$wGARCH), "Variable" = "Y_2"))
ddAllBoxPlot_wgarch <- boxplot_dists(df_dd_all_wgarch,
                                     k_min, k_max, k_brk, linf = linf, lsup = lsup,
                                     colors_bts$wGARCH,
                                     "", "", "")
df_dd_all_sgarch <- rbind(cbind(freq_df(df_dd_all_Y1$sGARCH), "Variable" = "Y_1"),
                          cbind(freq_df(df_dd_all_Y2$sGARCH), "Variable" = "Y_2"))
ddAllBoxPlot_sgarch <- boxplot_dists(df_dd_all_sgarch,
                                     k_min, k_max, k_brk, linf = linf, lsup = lsup,
                                     colors_bts$sGARCH,
                                     "", xtitle, "",
                                     flag = 0)

multiplot(ddAllBoxPlot_ibwn, ddAllBoxPlot_cbwn,
          ddAllBoxPlot_wvar, ddAllBoxPlot_svar,
          ddAllBoxPlot_wgarch, ddAllBoxPlot_sgarch,
          cols = 6)
save(ddAllBoxPlot_ibwn, ddAllBoxPlot_cbwn,
     ddAllBoxPlot_wvar, ddAllBoxPlot_svar,
     ddAllBoxPlot_wgarch, ddAllBoxPlot_sgarch,
     file = paste0("results/ddAllBoxPlot.RData"))



######################################################
### PointPlots of Degree Distribution set
semilog <- 1
log <- 0

### INTRA
k_min <- 2
k_max <- 22
linf <- 0 # 0.0001 # 0
lsup <- 0.35      # 1      # 0.35

## WN
pointplot_dists(df_dd_intra_ibwn,
                k_min, k_max, k_brk, linf = linf, lsup = lsup,
                colors_bts$iBWN,
                "", "", "P(k): Intra-Layer",
                is_semilog = semilog, is_log = log)


### INTER
df_dd_inter_Y1 <- df_dists(dd_inter_Y1, model_names, n_inst, 2, 14)
df_dd_inter_Y2 <- df_dists(dd_inter_Y2, model_names, n_inst, 2, 14)

### ALL
df_dd_all_Y1 <- df_dists(dd_all_Y1, model_names, n_inst, 4, 32)
df_dd_all_Y2 <- df_dists(dd_all_Y2, model_names, n_inst, 4, 32)

