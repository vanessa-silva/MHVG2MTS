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