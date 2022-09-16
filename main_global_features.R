#########################################################
##########       INTRA, INTER, ALL and         ##########
########    RELATIONAL layers GLOBAL Features    ########
#########################################################

### load libraries, auxiliary functions and DPG info's
source("libraries.R")
source("aux_code/utils_functions.R")
source("aux_code/plot_functions.R")

load("results/GlobalFeatures.RData")


######################################################
### Analyse Global Features

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



######################################################
### PCA analysis of features
norm <- FALSE   ## if is z-normalized
colors <- c("darkorange", "green3",  
            "cornflowerblue", "red",
            "cyan", "violetred1")

#########
### Intra
GlobalIntra_norm <- cbind(GlobalIntra_Y1_norm[, -ncol(GlobalIntra_Y1_norm)],
                          GlobalIntra_Y2_norm)
colnames(GlobalIntra_norm) <- c("k_1", "d_1", "S_1", "Q_1",
                                "k_2", "d_2", "S_2", "Q_2",
                                "Model")

pca_res_IntraFeatures <- pca_results(GlobalIntra_norm, col = colors,
                                     title = "Intra-layer MNet Features")
pca_res_IntraFeatures$PCAplots$pca_plot
pca_res_IntraFeatures$PCAplots$bar_plot

#########
### Inter
colnames(GlobalInter_norm) <- c("k_1,2", "d_1,2", "S_1,2", "Q_1,2",
                                "Model")

pca_res_InterFeatures <- pca_results(GlobalInter_norm, col = colors,
                                     title = "Inter-layer MNet Features")
pca_res_InterFeatures$PCAplots$pca_plot
pca_res_InterFeatures$PCAplots$bar_plot

#########
### All
colnames(GlobalAll_norm) <- c("k", "d", "S", "Q",
                              "Model")

pca_res_AllFeatures <- pca_results(GlobalAll_norm, col = colors,
                                   title = "All-layer MNet Features")
pca_res_AllFeatures$PCAplots$pca_plot
pca_res_AllFeatures$PCAplots$bar_plot

#########
### Relational
colnames(GlobalRelat_norm) <- c("r_1,2", "r_2,1",
                                "JS_intra", "JS_inter", "JS_all",
                                "Model")

pca_res_RelatFeatures <- pca_results(GlobalRelat_norm, col = colors,
                                     title = "Relational-layer MNet Features")
pca_res_RelatFeatures$PCAplots$pca_plot
pca_res_RelatFeatures$PCAplots$bar_plot

#########
### Intra, Inter, All and Relational Features
allFeatures <- cbind(GlobalIntra_norm[, -ncol(GlobalIntra_norm)],
                     GlobalInter_norm[, -ncol(GlobalInter_norm)],
                     GlobalAll_norm[, -ncol(GlobalAll_norm)],
                     GlobalRelat_norm)

pca_res_allFeatures <- pca_results(allFeatures, col = colors, 
                                   title = "MNet Features")
pca_res_allFeatures$PCAplots$pca_plot
pca_res_allFeatures$PCAplots$bar_plot


#########
## save PCA results
save(GlobalIntra_norm, GlobalInter_norm,
     GlobalAll_norm, GlobalRelat_norm,
     allFeatures,
     pca_res_IntraFeatures,
     pca_res_InterFeatures,
     pca_res_AllFeatures,
     pca_res_RelatFeatures,
     pca_res_allFeatures,
     file = paste0("results/PCA_results.RData"))
