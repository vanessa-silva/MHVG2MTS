#########################################################
###########       Clustering Analysis         ###########
#########################################################

### load libraries, auxiliary functions and DPG info's
source("libraries.R")
source("aux_code/utils_functions.R")
source("aux_code/plot_functions.R")


######################################################
### Feature clustering analysis
load("results/PCA_results.RData")
colors <- c("darkorange", "green3",  
            "cornflowerblue", "red",
            "cyan", "violetred1")
set.seed(062022)

#########
### Intra
clustering_res_IntraFeatures <- clustering_results(pca_res_IntraFeatures$PCA_res,
                                                   pca_res_IntraFeatures$PCA,
                                                   GlobalIntra_norm,
                                                   colors,
                                                   title = "Intra-layer Features")

multiplot(clustering_res_IntraFeatures$Best_k$plot.ari,
          clustering_res_IntraFeatures$Best_k$plot.nmi,
          clustering_res_IntraFeatures$Best_k$plot.sil,
          cols = 3)
clustering_res_IntraFeatures$ClusterPlot

#########
### Inter
clustering_res_InterFeatures <- clustering_results(pca_res_InterFeatures$PCA_res,
                                                   pca_res_InterFeatures$PCA,
                                                   GlobalInter_norm,
                                                   colors,
                                                   title = "Inter-layer Features")

multiplot(clustering_res_InterFeatures$Best_k$plot.ari,
          clustering_res_InterFeatures$Best_k$plot.nmi,
          clustering_res_InterFeatures$Best_k$plot.sil,
          cols = 3)
clustering_res_InterFeatures$ClusterPlot

#########
### All
clustering_res_AllFeatures <- clustering_results(pca_res_AllFeatures$PCA_res,
                                                 pca_res_AllFeatures$PCA,
                                                 GlobalAll_norm,
                                                 colors,
                                                 title = "All-layer Features")

multiplot(clustering_res_AllFeatures$Best_k$plot.ari,
          clustering_res_AllFeatures$Best_k$plot.nmi,
          clustering_res_AllFeatures$Best_k$plot.sil,
          cols = 3)
clustering_res_AllFeatures$ClusterPlot

#########
### Relational
clustering_res_RelatFeatures <- clustering_results(pca_res_RelatFeatures$PCA_res,
                                                   pca_res_RelatFeatures$PCA,
                                                   GlobalRelat_norm,
                                                   colors,
                                                   title = "Relational-layer Features")

multiplot(clustering_res_RelatFeatures$Best_k$plot.ari,
          clustering_res_RelatFeatures$Best_k$plot.nmi,
          clustering_res_RelatFeatures$Best_k$plot.sil,
          cols = 3)
clustering_res_RelatFeatures$ClusterPlot

#########
### Intra, Inter, All and Relational Features
clustering_res_allFeatures <- clustering_results(pca_res_allFeatures$PCA_res,
                                                 pca_res_allFeatures$PCA,
                                                 allFeatures,
                                                 colors,
                                                 title = "MNet Features")

multiplot(clustering_res_allFeatures$Best_k$plot.ari,
          clustering_res_allFeatures$Best_k$plot.nmi,
          clustering_res_allFeatures$Best_k$plot.sil,
          cols = 3)
clustering_res_allFeatures$ClusterPlot


#########
### table results
table_eval <- data.frame("ARI" = c(clustering_res_IntraFeatures$Best_k$ari[5],
                                   clustering_res_InterFeatures$Best_k$ari[5],
                                   clustering_res_AllFeatures$Best_k$ari[5],
                                   clustering_res_RelatFeatures$Best_k$ari[5],
                                   clustering_res_allFeatures$Best_k$ari[5]),
                         "NMI" = c(clustering_res_IntraFeatures$Best_k$nmi.sqrt[5],
                                   clustering_res_InterFeatures$Best_k$nmi.sqrt[5],
                                   clustering_res_AllFeatures$Best_k$nmi.sqrt[5],
                                   clustering_res_RelatFeatures$Best_k$nmi.sqrt[5],
                                   clustering_res_allFeatures$Best_k$nmi.sqrt[5]),
                         "AS" = c(clustering_res_IntraFeatures$Best_k$silhouette[5],
                                  clustering_res_InterFeatures$Best_k$silhouette[5],
                                  clustering_res_AllFeatures$Best_k$silhouette[5],
                                  clustering_res_RelatFeatures$Best_k$silhouette[5],
                                  clustering_res_allFeatures$Best_k$silhouette[5]))
row.names(table_eval) <- c("Intra-layer",
                           "Inter-layer",
                           "All-layer",
                           "Relational",
                           "MNet")


## beautify cluster plot
## clustering result to true k
clustrs <- clustering_res_allFeatures$Clustering$cluster_fit$cluster
a <- c()
for (i in 1:length(clustrs)) {
  if(clustrs[i] == 1)
    a[i] <- 1
  else if(clustrs[i] == 3)
    a[i] <- 2
  else if(clustrs[i] == 2)
    a[i] <- 3
  else if(clustrs[i] == 4)
    a[i] <- 4
  else if(clustrs[i] == 5)
    a[i] <- 5
  else if(clustrs[i] == 6)
    a[i] <- 6
  
}
clustering_res_allFeatures$Clustering$cluster_fit$cluster <- a
plots_clusters(allFeatures[, -ncol(allFeatures)],
               as.factor(allFeatures[, ncol(allFeatures)]),
               clustering_res_allFeatures$Clustering$cluster_fit,
               colors)

### analyse all results
multiplot(clustering_res_IntraFeatures$Best_k$plot.ari,
          clustering_res_IntraFeatures$Best_k$plot.nmi,
          clustering_res_IntraFeatures$Best_k$plot.sil,
          clustering_res_InterFeatures$Best_k$plot.ari,
          clustering_res_InterFeatures$Best_k$plot.nmi,
          clustering_res_InterFeatures$Best_k$plot.sil,
          clustering_res_AllFeatures$Best_k$plot.ari,
          clustering_res_AllFeatures$Best_k$plot.nmi,
          clustering_res_AllFeatures$Best_k$plot.sil,
          clustering_res_RelatFeatures$Best_k$plot.ari,
          clustering_res_RelatFeatures$Best_k$plot.nmi,
          clustering_res_RelatFeatures$Best_k$plot.sil,
          clustering_res_allFeatures$Best_k$plot.ari,
          clustering_res_allFeatures$Best_k$plot.nmi,
          clustering_res_allFeatures$Best_k$plot.sil,
          cols = 5)

#########
## save clustering results
save(clustering_res_IntraFeatures,
     clustering_res_InterFeatures,
     clustering_res_AllFeatures,
     clustering_res_RelatFeatures,
     clustering_res_allFeatures,
     table_eval,
     file = paste0("results/Clustering_results.RData"))
