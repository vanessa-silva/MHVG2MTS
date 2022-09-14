##############################################################
########### Auxiliary Functions to Data Sequences ############
##############################################################



## function to reorder data.frame by level
order_level <- function(data) {
  data$k <- as.integer(levels(data$k))
  data <- data[order(data$k), ]
  data$k <- as.factor(data$k)
  
  return(data)
}



## Compute distribution
comp_distribution <- function(seq_data) {
  dist <- table(seq_data) / length(seq_data)
  
  if (names(dist)[1] == "1")
    dist <- dist[2:length(dist)]
  
  dist <- dist %>% as.data.frame.table()
  
  return(dist)
}



## Compute a distribution of a given sample sequence
dist_degree <- function(seq_data, model_names, n_inst, col_idx) {
  
  ## get all distributions of BTS set
  dist <- list()
  for (m in 1:length(model_names)) {
    dist[[m]] <- list()
    
    for (i in 1:n_inst)
      dist[[m]][[i]] <- comp_distribution(seq_data[[m]][[i]][, col_idx])
  }
  names(dist) <- model_names
  
  ## join all distributions by model
  join_dist <- list()
  for (i in 1:n_inst) {
    join_dist[[i]] <- dist[[1]][[i]]
    
    for (m in 2:length(model_names)) {
      aux <- dist[[m]][[i]]
      join_dist[[i]] <- full_join(join_dist[[i]],
                                  aux,
                                  by = c("seq_data"))
    }
  }
  ## sort order degree
  for (i in 1:n_inst) {
    colnames(join_dist[[i]]) <- c("k", model_names)
    join_dist[[i]] <- order_level(join_dist[[i]])
  }
  
  ## return degree distribution
  return(join_dist)
}



## Compute mean of distribution set
mean_dist_degree <- function(dist_data, n_inst) {
  melt_dist <- dist_data[[1]]
  
  for (i in 2:n_inst)
    melt_dist <- rbind(melt_dist, dist_data[[i]])
  print(table(melt_dist$k))
  
  mean_dist <- melt_dist %>%
    group_by(k) %>%
    summarise_all(mean, na.rm = TRUE) %>%
    as.data.frame()
  
  ## sort order degree
  mean_dist <- order_level(mean_dist)
  
  return(mean_dist)
}



## Compute standard deviation of distribution set
sd_dist_degree <- function(dist_data, n_inst) {
  melt_dist <- dist_data[[1]]
  
  for (i in 2:n_inst)
    melt_dist <- rbind(melt_dist, dist_data[[i]])
  
  sd_dist <- melt_dist %>%
    group_by(k) %>%
    summarise_all(sd, na.rm = TRUE) %>%
    as.data.frame()
  
  ## sort order degree
  sd_dist <- order_level(sd_dist)
  
  return(sd_dist)
}

