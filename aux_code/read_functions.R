##############################################################
########### Auxiliary Functions to Read Files Data ###########
##############################################################



## Read measures from files .txt of a 1 instance MTS
read_measures <- function(dir, class_name, tmeasure_name, layer_A, layer_B = 0) {
  if(layer_B == 0)
    file_name <- paste0(dir, "/", class_name, "__", layer_A, "_", tmeasure_name, ".txt")
  else
    file_name <- paste0(dir, "/", class_name, "__", layer_A, "-", layer_B,  "_", tmeasure_name, ".txt")
  #print(paste("--->", file_name))
  
  data <- read.table(file_name, header = F, sep = ",")
  data <- t(data)
  data <- as.data.frame(data)
  colnames(data) <- as.character(data[1, ])
  data <- data[-1, ]

  for(i in 1:ncol(data))
    data[, i] <- as.numeric(data[, i])
  
  print(colnames(data))

  return(data)
}


## Read measures from files .txt of a rep instances MTS
read_measures_set <- function(dir, class_name, tmeasure_name, n, layer_A, layer_B = 0) {
  if(layer_B == 0)
    file_name <- paste0(dir, "/", class_name, "_", n, "__", layer_A, "_", tmeasure_name, ".txt")
  else
    file_name <- paste0(dir, "/", class_name, "_", n, "__", layer_A, "-", layer_B,  "_", tmeasure_name, ".txt")

  data <- read.table(file_name, header = F, sep = ",")
  data <- t(data)
  data <- as.data.frame(data)
  colnames(data) <- as.character(data[1, ])
  data <- data[-1, ]

  if(class(data) == "character") {
    data <- as.numeric(data)
    data <- data.frame("AvgDegree" = data)
  }
  else {
    for(i in 1:ncol(data))
      data[, i] <- as.numeric(data[, i])

    data$Class <- class_name
  }

  return(data)
}
