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
freq_df <- function(df_data) {
  dd <- data.frame("Freq" = df_data[, 1], 
                   "k" = as.integer(colnames(df_data)[1]))
  
  for(i in 2:ncol(df_data)) {
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
