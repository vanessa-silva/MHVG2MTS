#########################################################
##########       INTRA, INTER, ALL and         ##########
########    RELATIONAL layers GLOBAL Features    ########
#########################################################

### load libraries, auxiliary functions and DPG info's
source("libraries.R")
source("info_data.R")
source("aux_code/utils_functions.R")
source("aux_code/plot_functions.R")


######################################################
### Analyse Global Features
load("results/GlobalFeatures.RData")


### INTRA
## no-normalized
draw_tables(GlobalIntra_mean, colors = 1)
draw_tables(GlobalIntra_sd, colors = 0)
## nnormalized
draw_tables(GlobalIntra_norm_mean, colors = 1)
draw_tables(GlobalIntra_norm_sd, colors = 0)

### INTER
## no-normalized
draw_tables(GlobalInter_mean, colors = 1)
draw_tables(GlobalInter_sd, colors = 0)
## normalized
draw_tables(GlobalInter_norm_mean, colors = 1)
draw_tables(GlobalInter_norm_sd, colors = 0)

### ALL
## no-normalized
draw_tables(GlobalAll_mean, colors = 1, latex = 0)
draw_tables(GlobalAll_sd, colors = 0, latex = 0)
## normalized
draw_tables(GlobalAll_norm_mean, colors = 1, latex = 0)
draw_tables(GlobalAll_norm_sd, colors = 0, latex = 0)

### RELATIONAL
## no-normalized
draw_tables(GlobalRelat_mean, colors = 1, latex = 0)
draw_tables(GlobalRelat_sd, colors = 0, latex = 0)
## normalized
draw_tables(GlobalRelat_norm_mean, colors = 1, latex = 0)
draw_tables(GlobalRelat_norm_sd, colors = 0, latex = 0)



######################################################
### Boxplot of features
model_level <- c("iBWN", "cBWN", 
                 "wVAR", "sVAR",
                 "wGARCH", "sGARCH")
colors <- c("green3", "darkorange", 
            "violetred1", "red",
            "cyan", "cornflowerblue")

### INTRA
## Y1
bp_intra_Y1_ad <- ggplot_models(GlobalIntra_Y1, GlobalIntra_Y1$Model, model_level,
                                "AvgDegree", "Intra Avg. Degree (Y1)", colors)
bp_intra_Y1_apl <- ggplot_models(GlobalIntra_Y1, GlobalIntra_Y1$Model, model_level,
                                 "AvgPathLength", "Intra Avg. Path Leng. (Y1)", colors)
bp_intra_Y1_nc <- ggplot_models(GlobalIntra_Y1, GlobalIntra_Y1$Model, model_level,
                                "NumCommunities", "Intra Num. Communities (Y1)", colors)
bp_intra_Y1_m <- ggplot_models(GlobalIntra_Y1, GlobalIntra_Y1$Model, model_level,
                               "Modularity", "Intra Modularity (Y1)", colors)
## Y2
bp_intra_Y2_ad <- ggplot_models(GlobalIntra_Y2, GlobalIntra_Y2$Model, model_level,
                                "AvgDegree", "Intra Avg. Degree (Y2)", colors)
bp_intra_Y2_apl <- ggplot_models(GlobalIntra_Y2, GlobalIntra_Y2$Model, model_level,
                                 "AvgPathLength", "Intra Avg. Path Leng. (Y2)", colors)
bp_intra_Y2_nc <- ggplot_models(GlobalIntra_Y2, GlobalIntra_Y2$Model, model_level,
                                "NumCommunities", "Intra Num. Communities (Y2)", colors)
bp_intra_Y2_m <- ggplot_models(GlobalIntra_Y2, GlobalIntra_Y2$Model, model_level,
                               "Modularity", "Intra Modularity (Y2)", colors)
### INTER
bp_inter_ad <- ggplot_models(GlobalInter, GlobalInter$Model, model_level,
                             "AvgDegree", "Inter Avg. Degree", colors)
bp_inter_apl <- ggplot_models(GlobalInter, GlobalInter$Model, model_level,
                              "AvgPathLength", "Inter Avg. Path Leng.", colors)
bp_inter_nc <- ggplot_models(GlobalInter, GlobalInter$Model, model_level,
                             "NumCommunities", "Inter Num. Communities", colors)
bp_inter_m <- ggplot_models(GlobalInter, GlobalInter$Model, model_level,
                            "Modularity", "Inter Modularity", colors)
### ALL
bp_all_ad <- ggplot_models(GlobalAll, GlobalAll$Model, model_level,
                           "AvgDegree", "All Avg. Degree", colors)
bp_all_apl <- ggplot_models(GlobalAll, GlobalAll$Model, model_level,
                            "AvgPathLength", "All Avg. Path Leng.", colors)
bp_all_nc <- ggplot_models(GlobalAll, GlobalAll$Model, model_level,
                           "NumCommunities", "All Num. Communities", colors)
bp_all_m <- ggplot_models(GlobalAll, GlobalAll$Model, model_level,
                          "Modularity", "All Modularity", colors)

multiplot(bp_intra_Y1_ad, bp_intra_Y1_apl, bp_intra_Y1_nc, bp_intra_Y1_m,
          bp_intra_Y2_ad, bp_intra_Y2_apl, bp_intra_Y2_nc, bp_intra_Y2_m,
          bp_inter_ad, bp_inter_apl, bp_inter_nc, bp_inter_m, 
          bp_all_ad, bp_all_apl, bp_all_nc, bp_all_m, 
          cols = 4)

### RELATIONAL
bp_relat_ard1 <- ggplot_models(GlobalRelat, GlobalRelat$Model, model_level,
                               "AvgRatDegree(1-2)", "Avg. Rat. Degree (Y1)", colors)
bp_relat_ard2 <- ggplot_models(GlobalRelat, GlobalRelat$Model, model_level,
                               "AvgRatDegree(2-1)", "Avg. Rat. Degree (Y2)", colors)
bp_relat_jsintra <- ggplot_models(GlobalRelat, GlobalRelat$Model, model_level,
                                  "JSintraDegree(1-2)", "Intra JSD", colors)
bp_relat_jsinter <- ggplot_models(GlobalRelat, GlobalRelat$Model, model_level,
                                  "JSinterDegree(1-2)", "Inter JSD", colors)
bp_relat_jsall <- ggplot_models(GlobalRelat, GlobalRelat$Model, model_level,
                                "JSDegree(1-2)", "All JSD", colors)

multiplot(bp_relat_ard1, bp_relat_ard2, 
          bp_relat_jsintra, bp_relat_jsinter,
          bp_relat_jsall,  
          cols = 5)
