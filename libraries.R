#####################################################
##### load needed libraries
#####################################################

## utils 
if (!require(dplyr)) 
  install.packages('dplyr')
if (!require(reshape2))
  install.packages('reshape2')

## time series
if (!require(forecast))
  install.packages('forecast')

## tables
if (!require(kableExtra))
  install.packages('kableExtra')
if (!require(formattable))
  install.packages('formattable')

## plots
if (!require(ggplot2))
  install.packages('ggplot2')
if (!require(corrplot))
  install.packages('corrplot')

## PCA
if (!require(factoextra))
  install.packages('factoextra')

## clustering
if (!require(cluster))
  install.packages('cluster')

## evaluation measures
if (!require(aricode))
  install.packages('aricode')

