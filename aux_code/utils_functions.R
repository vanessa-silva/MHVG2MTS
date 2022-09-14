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
