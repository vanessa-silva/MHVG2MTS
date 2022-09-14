#########################################################
#################      DRAW PLOTS      ##################
#########################################################

### load libraries and auxiliary functions
source("libraries.R")
source("aux_code/plot_functions.R")


### draw plot of Figure 6 of the paper

load("results/bts_plots.RData")     ## BTS's plots
load("results/acf_plots.RData")     ## ACF's plots
load("results/ccf_plots.RData")     ## CCF's plots

### choose scale type
semilog <- 1
log <- 0
scale_type <- "scalefree"
if(semilog)
  scale_type <- "semilogscale"
if(log)
  scale_type <- "logscale"
load(paste0("results/ddIntra_", scale_type, "_plots.RData"))     ## Intra DD's plots
load(paste0("results/ddInter_", scale_type, "_plots.RData"))     ## Inter DD's plots
load(paste0("results/ddAll_", scale_type, "_plots.RData"))       ## All DD's plots

### joint all plots
multiplot(
  ## MTS
  plot_ibwn, plot_cbwn,
  plot_wvar, plot_svar,
  plot_wgarch, plot_sgarch,
  ## CCF
  ccfPlot_ibwn, ccfPlot_cbwn,
  ccfPlot_wvar, ccfPlot_svar,
  ccfPlot_wgarch, ccfPlot_sgarch,
  ## DD INTRA
  ddIntraPlot_ibwn, ddIntraPlot_cbwn,
  ddIntraPlot_wvar, ddIntraPlot_svar,
  ddIntraPlot_wgarch, ddIntraPlot_sgarch,
  ## DD INTER
  ddInterPlot_ibwn, ddInterPlot_cbwn,
  ddInterPlot_wvar, ddInterPlot_svar,
  ddInterPlot_wgarch, ddInterPlot_sgarch,
  ## DD ALL
  ddAllPlot_ibwn, ddAllPlot_cbwn,
  ddAllPlot_wvar, ddAllPlot_svar,
  ddAllPlot_wgarch, ddAllPlot_sgarch,
  ## column numbers
  cols = 5)
