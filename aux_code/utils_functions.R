##############################################################
##################### Auxiliary Functions ####################
##############################################################



## Melt 2 data frames
melt_variables <- function(df1, df2) {
  
  aux_df <- full_join(df1, df2, by = c("k"))
  colnames(aux_df) <- c("k", "Y_1", "Y_2")
  
  df <- melt(aux_df, id.vars = 'k', variable.name = 'Variable')
  
  return(df)
}



## list of data.frames with sets of degree distributions 
df_dists <- function(list_data, model_names, n_inst, k_f, k_l) {
  list_df <- list()
  degs <- as.character(seq(k_f, k_l, 1))
  
  for (m in 1:length(model_names)) {
    for (nk in 1:length(degs)) {
      
      for (i in 1:n_inst) {
        if(i == 1) {
          aux <- list_data[[i]][list_data[[i]]$k == degs[nk], m+1]
          if(length(aux) == 0)
            aux <- NA  
        }
        else {
          aux2 <- list_data[[i]][list_data[[i]]$k == degs[nk], m+1]
          if(length(aux2) == 0)
            aux2 <- NA
          
          aux <- c(aux, aux2)
        }
      }
      
      if (nk == 1)
        list_df[[m]] <- data.frame(aux)
      else
        list_df[[m]] <- cbind(list_df[[m]], aux)
      
      colnames(list_df[[m]])[nk] <- degs[nk]
      
    }
  }
  
  names(list_df) <- model_names
  
  return(list_df)
} 



## join all frequencies of dataframe of distributions
freq_df <- function(df_data, flag = 0) {
  if (flag)
    dd <- data.frame("Freq" = df_data[, 1], 
                     "k" = 2)
  else
    dd <- data.frame("Freq" = df_data[, 1],
                     "k" = as.integer(colnames(df_data)[1]))
  
  for(i in 2:ncol(df_data)) {
    if (flag)
      dd <- rbind(dd, data.frame("Freq" = df_data[, i], 
                                 "k" = i+1))
    else
      dd <- rbind(dd, data.frame("Freq" = df_data[, i],
                                 "k" = as.integer(colnames(df_data)[i])))
  }
  
  dd$k <- as.factor(dd$k)
  
  return(dd)
}



## generating table with(out) colors degree's
draw_tables <- function(table_data, colors = 0, latex = 0) {
  table_data <- table_data %>% round(5)
  if(colors) {
    if(latex) {
      table_data %>%
        mutate_at(colnames(table_data), color_tile("white", "red")) %>%
        select(everything()) %>%
        kable("latex", escape = F) %>%
        kable_styling(bootstrap_options = c("hover", "condensed")) %>%
        column_spec(1:ncol(table_data), color = "black") %>%
        row_spec(0, bold = T, color = "black")
    }
    else {
      table_data %>%
        mutate_at(colnames(table_data), color_tile("white", "red")) %>%
        select(everything()) %>%
        kable("html", escape = F, table.attr = "style='width:100%;'") %>%
        kable_styling(bootstrap_options = c("hover", "condensed")) %>%
        column_spec(1:ncol(table_data), color = "black") %>%
        row_spec(0, bold = T, color = "black")
    }
  }
  else {
    if(latex) {
      table_data %>%
        kable("latex", escape = F) %>%
        kable_styling(bootstrap_options = c("hover", "condensed")) %>%
        column_spec(1:ncol(table_data), color = "black") %>%
        row_spec(0, bold = T, color = "black")
    }
    else {
      table_data %>%
        kable("html", escape = F, table.attr = "style='width:100%;'") %>%
        kable_styling(bootstrap_options = c("hover", "condensed")) %>%
        column_spec(1:ncol(table_data), color = "black") %>%
        row_spec(0, bold = T, color = "black")
    }
  }
}



## computing PCA
comp_pca <- function(data, norm = FALSE, prec = 1) {
  
  ##calculate PCA
  pca <- prcomp(data, center = TRUE, scale. = norm)
  
  #decide number of components
  cum_prop <- summary(pca)$importance[3, ]
  if(which(cum_prop >= prec)[1] == 1)
    ncp <- which(cum_prop >= prec)[2]
  else
    ncp <- which(cum_prop >= prec)[1]
  
  res <- list("pca" = pca, 
              "impor_components" = cum_prop, 
              "n_components" = ncp)
  return(res)
}



## generating module to compute PCA analysis
pca_results <- function(data, col, norm = FALSE, prec = 1, title = "MNet Features") {
  
  pca_res <- comp_pca(data[, -ncol(data)], norm)
  pca <- pca_res$pca
  pcaplots <- plots_pca(pca,
                        as.factor(data[, ncol(data)]),
                        pca_res$n_components,
                        col, title = title)
  
  res <- list("PCA_res" = pca_res,
              "PCA" = pca,
              "PCAplots" = pcaplots)
  return(res)
}



## compute KMeans function
comp_clusters <- function(data_redim, k, true_classes) {
  ## compute k-means
  k.means.fit <- kmeans(data_redim, k,
                        nstart = 50, iter.max = 1000)
  
  ## confusion matrix
  conf_matrix <- table(true_classes, k.means.fit$cluster)
  
  ## compute silhouette metric
  sill <- silhouette(k.means.fit$cluster, dist(data_redim))
  
  ## get adjusted Rand index
  ari <- ARI(k.means.fit$cluster, true_classes)
  
  ## get normalized mutual information
  nmi_sqrt <- NMI(k.means.fit$cluster, true_classes,
                  variant = "sqrt")
  
  res <- list("cluster_fit" = k.means.fit,
              "confusion_mtx" = conf_matrix,
              "silhouette" = sill,
              "ari" = ari,
              "nmi.sqrt" = nmi_sqrt)
  
  return(res)
}



## generating module to compute clustering analysis
clustering_results <- function(pca_res, pca, data, col, title) {
  ## find best k (number of clustering)
  best_k <- clust_det(data.frame(pca$x[, 1:pca_res$n_components]),
                      data,
                      title)
  
  clusters <- comp_clusters(data.frame(pca$x[, 1:pca_res$n_components]),
                            length(unique(data[, ncol(data)])),
                            as.factor(data[, ncol(data)]))
  
  clustersplots <- plots_clusters(data[, -ncol(data)],
                                  as.factor(data[, ncol(data)]),
                                  clusters$cluster_fit,
                                  col)
  
  
  res <- list("Best_k" = best_k,
              "Clustering" = clusters,
              "ClusterPlots" = clustersplots)
}



## determinate best number of clusters function
clust_det <- function(pca_data, data, title) {
  true_classes <- as.factor(data[, ncol(data)])
  
  ari <- (nrow(pca_data) - 1)*sum(apply(pca_data, 2, var))
  nmi_sqrt <- (nrow(pca_data) - 1)*sum(apply(pca_data, 2, var))
  sil <- (nrow(pca_data) - 1)*sum(apply(pca_data, 2, var))
  
  ari[1] <- 0
  nmi_sqrt[1] <- 0
  sil[1] <- 0
  
  max_k <- (length(unique(true_classes))+6)
  for (i in 2:max_k) {
    aux_ari <- c()
    aux_nmi_sqrt <- c()
    aux_sil <- c()
    
    for(j in 1:10) {
      k.means.fit <- kmeans(pca_data, centers = i, nstart = 50, iter.max = 1000)
      
      aux_ari[j] <- ARI(k.means.fit$cluster, true_classes)
      aux_nmi_sqrt[j] <- NMI(k.means.fit$cluster, true_classes)
      aux_sil[j] <- mean(silhouette(k.means.fit$cluster, dist(pca_data))[,3])
    }
    
    if(i == 2) {
      e_metrics_ari <- data.frame(aux_ari)
      e_metrics_nmi_sqrt <- data.frame(aux_nmi_sqrt)
      e_metrics_sil <- data.frame(aux_sil)
    }
    else {
      e_metrics_ari <- cbind(e_metrics_ari, aux_ari)
      e_metrics_nmi_sqrt <- cbind(e_metrics_nmi_sqrt, aux_nmi_sqrt)
      e_metrics_sil <- cbind(e_metrics_sil, aux_sil)
    }
    
    ari[i] <- round(mean(aux_ari), 3)
    nmi_sqrt[i] <- round(mean(aux_nmi_sqrt), 3)
    sil[i] <- round(mean(aux_sil), 3)
  }
  
  g_ari <- boxplot_clust(freq_df(e_metrics_ari, flag = 1), 
                         y_title = "Adj. Rand Index", 
                         main_title = title)
  g_nmi_sqrt <- boxplot_clust(freq_df(e_metrics_nmi_sqrt, flag = 1), 
                              y_title = "Norm. Mutual Information", 
                              main_title = title)
  g_sil <- boxplot_clust(freq_df(e_metrics_sil, flag = 1), 
                         y_title = "Average Silhouette", 
                         main_title = title)
    
  res <- list("silhouette" = sil,
              "ari" = ari,
              "nmi.sqrt" = nmi_sqrt,
              "plot.ari" = g_ari,
              "plot.nmi" = g_nmi_sqrt,
              "plot.sil" = g_sil)
  
  return(res)
}
