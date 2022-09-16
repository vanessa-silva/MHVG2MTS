##############################################################
############# Auxiliary Functions to Draw Plots ##############
##############################################################



## MTS plot
mts_plot <- function(mts, min, max, brk, cols, 
                     y_title = "", x_title = "", main_title = "", 
                     flag = 0) {
  melt_mts <- melt(mts, id = "Time")
  
  g <- ggplot(melt_mts,
              aes(x = Time, y = value,
                  group = variable,
                  colour = variable)) +
    geom_line() +
    scale_color_manual(values = cols) +
    geom_point(size = 0.7) +
    theme(axis.text.x = element_text(size = 8, face = "bold"),
          axis.text.y = element_text(size = 8, face = "bold"),
          axis.title.x = element_text(size = 11, face = "bold"),
          axis.title.y = element_text(size = 11, face = "bold"),
          plot.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 8, face = "bold"),
          legend.position = "none",
          panel.background = element_rect(fill = "gray97",
                                          colour = "black",
                                          linetype = "solid",
                                          size = 0.5))
  
  ## y-axis title
  if(y_title == "")
    g <- g + ylab(NULL)
  else
    g <- g + scale_y_continuous(name = y_title)
  ## x-axis title
  if(x_title == "")
    g <- g + xlab(NULL)
  else
    g <- g + xlab(x_title)
  ## main title
  if(flag)
    g <- g + ggtitle(NULL)
  else
    g <- g + ggtitle(main_title)
  
  return(g)
}



## ACF plot
acf_plot <- function(ts, y_title = "", x_title = "", main_title = "", 
                     linf = -1, lsup = -1) {
  g <- ggAcf(ts, lag.max = 15) +
    ggtitle(main_title) +
    theme(axis.text.x = element_text(size = 8, face = "bold"),
          axis.text.y = element_text(size = 8, face = "bold"),
          axis.title.x = element_text(size = 11, face = "bold"),
          axis.title.y = element_text(size = 11, face = "bold"),
          plot.title = element_text(size = 12, face = "bold"),
          panel.background = element_rect(fill = "gray97",
                                          colour = "black",
                                          linetype = "solid",
                                          size = 0.5))
  
  ## y-axis title
  if(y_title == "")
    g <- g + ylab(NULL)
  else
    g <- g + ylab(y_title)
  ## x-axis title
  g <- g + xlab(x_title)
  
  ## y-axis limits
  if(linf != lsup)
    g <- g + scale_y_continuous(limits = c(linf, lsup))
  
  return(g)
}



## CCF plot
ccf_plot <- function(ts_a, ts_b, y_title = "", x_title = "", main_title = "", 
                     linf = -1, lsup = -1, flag = 0) {
  g <- ggCcf(ts_a, ts_b, lag.max = 15) +
    theme(axis.text.x = element_text(size = 8, face = "bold"),
          axis.text.y = element_text(size = 8, face = "bold"),
          axis.title.x = element_text(size = 11, face = "bold"),
          axis.title.y = element_text(size = 11, face = "bold"),
          plot.title = element_text(size = 12, face = "bold"),
          panel.background = element_rect(fill = "gray97",
                                          colour = "black",
                                          linetype = "solid",
                                          size = 0.5))
  
  ## y-axis title
  if(y_title == "")
    g <- g + ylab(NULL)
  else
    g <- g + scale_y_continuous(name = y_title)
  ## x-axis title
  if(x_title == "")
    g <- g + xlab(NULL)
  else
    g <- g + xlab(x_title)
  ## main title
  if(flag)
    g <- g + ggtitle(NULL)
  else
    g <- g + ggtitle(main_title)
  
  ## y-axis limits
  if(linf != lsup)
    g <- g + scale_y_continuous(limits = c(linf, lsup))
  
  return(g)
}





## Distribution plot
dist_plot <- function(melt_data, min, max, brk, linf, lsup, cols, 
                      y_title = "", x_title = "", main_title = "",
                      is_smooth = 0, is_semilog = 0, is_log = 0, flag = 0) {
  if(is_log) {
    melt_data$k <- as.numeric(as.character(melt_data$k))
    melt_data$k <- log10(melt_data$k)
  }
  
  g <- ggplot(melt_data,
              aes(x = k, y = value,
                  group = Variable)) +
    geom_line(aes(colour = Variable)) +
    geom_point(aes(colour = Variable)) +
    scale_color_manual(values = cols) +
    theme(axis.text.x = element_text(size = 8, face = "bold"),
          axis.text.y = element_text(size = 8, face = "bold"),
          legend.text = element_text(size = 8, face = "bold"),
          axis.title.x = element_text(size = 11, face = "bold"),
          axis.title.y = element_text(size = 11, face = "bold"),
          plot.title = element_text(size = 12, face = "bold"),
          legend.title = element_blank(),
          legend.position=c(c(0.8, 0.85)),
          panel.background = element_rect(fill = "gray97",
                                          colour = "black",
                                          size = 0.5, linetype = "solid"))
  
  ## x-axis title
  if(x_title == "")
    g <- g + xlab(NULL)
  else
    g <- g + xlab(x_title)
  ## y-axis title
  if(y_title == "")
    g <- g + ylab(NULL)
  else
    g <- g + ylab(y_title)
  ## main title
  if(flag)
    g <- g + ggtitle(NULL)
  else
    g <- g + ggtitle(main_title)
  
  ## axis scale
  if(is_semilog) {
    if(linf != lsup)
      g <- g + scale_y_log10(limits = c(linf, lsup))
    else
      g <- g + scale_y_log10()
    g <- g + scale_x_discrete(breaks = c(seq(min, max, by = brk)))
  }
  else if(is_log) {
    if(linf != lsup)
      g <- g + scale_y_log10(limits = c(linf, lsup))
    else
      g <- g + scale_y_log10()
    x_min <- round(min(melt_data$k), 2)
    x_max <- round(max(melt_data$k)+0.03, 2)
    g <- g + scale_x_continuous(limits = c(x_min, x_max))
  }
  else {
    if(linf != lsup)
      g <- g + scale_y_continuous(limits = c(linf, lsup))
    g <- g + scale_x_discrete(breaks = c(seq(min, max, by = brk)))
  }
  
  ## smooth the curve
  if(is_smooth)
    g <- g + geom_smooth(aes(colour = Variable))
  
  return(g)
}



## Boxplot of dataframe of distributions
boxplot_dists <- function(freq_data, min, max, brk, linf, lsup, cols,
                          y_title = "", x_title = "", main_title = "",
                          flag = 0) {
  g <- ggplot(data = freq_data, 
              aes(y = Freq, x = k,
                  fill = Variable)) + 
    geom_boxplot(alpha = 0.7) +
    scale_fill_manual(values = cols) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8, face = "bold"),
          axis.text.y = element_text(size = 8, face = "bold"),
          legend.text = element_text(size = 8, face = "bold"),
          axis.title.x = element_text(size = 11, face = "bold"),
          axis.title.y = element_text(size = 11, face = "bold"),
          plot.title = element_text(size = 12, face = "bold"),
          legend.title = element_blank(),
          legend.position=c(c(0.8, 0.85)),
          panel.background = element_rect(fill = "gray97",
                                          colour = "black",
                                          size = 0.5, linetype = "solid"))
  
  ## x-axis title
  # if(x_title == "")
  #   g <- g + 
  #     scale_x_discrete(breaks = c(seq(min, max, by = brk)), name = NULL)
  # else
    g <- g + 
      scale_x_discrete(breaks = c(seq(min, max, by = brk)), name = x_title)
  ## y-axis title
  if(y_title == "")
    g <- g + 
      scale_y_continuous(limits = c(linf, lsup), name = NULL)
  else
    g <- g + 
      scale_y_continuous(limits = c(linf, lsup), name = y_title)
  ## main title
  if(flag)
    g <- g + ggtitle(NULL)
  else
    g <- g + ggtitle(main_title)
  
  return(g)
}



## Pointplot of dataframe of distributions
pointplot_dists <- function(freq_data, min, max, brk, linf, lsup, cols, 
                            y_title = "", x_title = "", main_title = "",
                            is_smooth = 0, is_semilog = 0, is_log = 0, flag = 0) {
  if(is_log) {
    freq_data$k <- as.numeric(as.character(freq_data$k))
    freq_data$k <- log10(freq_data$k)
  }
  
  g <- ggplot(data = freq_data, 
              aes(y = Freq, x = k,
                  group = Variable)) + 
    geom_point(aes(colour = Variable)) +
    scale_color_manual(values = cols) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8, face = "bold"),
          axis.text.y = element_text(size = 8, face = "bold"),
          legend.text = element_text(size = 8, face = "bold"),
          axis.title.x = element_text(size = 11, face = "bold"),
          axis.title.y = element_text(size = 11, face = "bold"),
          plot.title = element_text(size = 12, face = "bold"),
          legend.title = element_blank(),
          legend.position=c(c(0.8, 0.8)),
          panel.background = element_rect(fill = "gray97",
                                          colour = "black",
                                          size = 0.5, linetype = "solid"))
  
  ## x-axis title
  if(x_title == "")
    g <- g + xlab(NULL)
  else
    g <- g + xlab(x_title)
  ## y-axis title
  if(y_title == "")
    g <- g + ylab(NULL)
  else
    g <- g + ylab(y_title)
  ## main title
  if(flag)
    g <- g + ggtitle(NULL)
  else
    g <- g + ggtitle(main_title)
  
  ## axis scale
  if(is_semilog) {
    if(linf != lsup)
      g <- g + scale_y_log10(limits = c(linf, lsup))
    else
      g <- g + scale_y_log10()
    g <- g + scale_x_discrete(breaks = c(seq(min, max, by = brk)))
  }
  else if(is_log) {
    if(linf != lsup)
      g <- g + scale_y_log10(limits = c(linf, lsup))
    else
      g <- g + scale_y_log10()
    x_min <- round(min(freq_data$k), 2)
    x_max <- round(max(freq_data$k)+0.03, 2)
    g <- g + scale_x_continuous(limits = c(x_min, x_max))
  }
  else {
    if(linf != lsup)
      g <- g + scale_y_continuous(limits = c(linf, lsup))
    g <- g + scale_x_discrete(breaks = c(seq(min, max, by = brk)))
  }
  
  ## smooth the curve
  if(is_smooth)
    g <- g + geom_smooth(aes(colour = Variable))
  
  return(g)
}





## function to draw boxplots by class models
### data          - is a data.frame with columns: features and class
### models        - is a array with class models
### level_models  - is a array with the level/order of unique class models
### feature       - column name of the feature to draw in boxplot
### title         - the title plot
### cols          - a array of colors by unique class models
ggplot_models <- function(data, models, level_models, feature, title, cols) {
  # manual levels
  data$models2 <- factor(models, levels = level_models)
  
  g <- ggplot(data, aes(x = models2, y = feature, fill = models2)) +
    geom_boxplot(alpha = 0.7) +
    scale_x_discrete(name = "Model") +
    scale_y_continuous(name = title) +
    scale_fill_manual(values = cols) +
    theme(axis.text.x = element_text(size = 13, angle = 15, face="bold"),
          axis.text.y = element_text(face="bold"),
          axis.title.x = element_text(size = 14, face="bold"),
          axis.title.y = element_text(size = 15, face="bold"),
          legend.position="none",
          panel.background = element_rect(fill = "gray97",
                                          colour = "black",
                                          size = 0.5, linetype = "solid"),)
  
  return(g)
}
### example of usage:  
##  g <- ggplot_models(metrics, models, level_models, feature, title, cols)
##  g <- ggplot_models(metrics[-wo, ], models[-wo], feature, title, cols)





## draw PCA plot
plots_pca <- function(pca, true_classes, ncp, col, title = "MNet Features") {
  ## draw two-component plot: Biplot of individuals of variables
  biplot <- fviz_pca_biplot(pca,
                            palette = col,
                            geom.ind = "point",
                            fill.ind = true_classes,
                            col.ind = "black",
                            pointshape = 21,
                            pointsize = 2,
                            addEllipses = TRUE,
                            labelsize = 5,
                            #alpha.var ="contrib",
                            col.var = "contrib",
                            gradient.cols = c("#F0E009", "#DC5F96", "#604B70"), ## yellow - violet - purple
                            repel = TRUE,
                            legend.title = list(fill = "Models",
                                                color = "Contrib"#, alpha = "Contrib"
                                                )
  ) + 
    theme(axis.text.x = element_text(size = 8, face = "bold"),
          axis.text.y = element_text(size = 8, face = "bold"),
          axis.title.x = element_text(size = 14, face="bold"),
          axis.title.y = element_text(size = 14, face="bold"),
          legend.text = element_text(size = 9, face = "bold"),
          legend.title = element_text(size = 10, face = "bold"),
          plot.title = element_text(size = 16, face = "bold"),
          panel.background = element_rect(fill = "gray97",
                                          colour = "black",
                                          size = 0.5, linetype = "solid"))
  
  ## draw contribution of variables
  var <- get_pca_var(pca)
  corrplot <- corrplot(var$contrib[, 1:as.integer(ncp)], 
                       is.corr = FALSE, tl.srt = 45)
  
  ## draw total contribution of PC's
  barplot <- fviz_contrib(pca, choice = "var", 
                          axes = 1:3, top = 10, 
                          fill = "#48D1CC", color = "black") +
    ggtitle("Contribution of Variables to All Dimensions") +
    xlab(title) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, face = "bold"),
          axis.text.y = element_text(size = 10, face = "bold"),
          axis.title.x = element_text(size = 12, face="bold"),
          axis.title.y = element_text(size = 12, face="bold"),
          plot.title = element_text(size = 14, face = "bold"),
          panel.background = element_rect(fill = "gray97",
                                          colour = "black",
                                          size = 0.5, linetype = "solid"))
  
  res <- list("pca_plot" = biplot,
              "corrplot" = corrplot,
              "bar_plot" = barplot)
  
  return(res)
}





## Boxplot of dataframe with clustring evaluation measures results
boxplot_clust <- function(df_data, y_title = "", main_title = "") {
  g <- ggplot(data = df_data, aes(y = Freq, x = k)) +
    geom_boxplot() +
    scale_y_continuous(name = y_title) +
    scale_x_discrete(name = "Number of Clusters (k)") +
    ggtitle(main_title) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 14, face="bold"),
          axis.text.y = element_text(size = 14, face="bold"),
          axis.title.x = element_text(size = 16, face="bold"),
          axis.title.y = element_text(size = 16, face="bold"),
          legend.position="none",
          plot.title = element_text(size = 16, face = "bold.italic"),
          panel.background = element_rect(fill = "white",
                                          colour = "grey80",
                                          size = .5, linetype = "solid"))
  
  return(g)
}



## draw jitterplot of the clusters
plots_clusters <- function(data, true_classes, cluster_fit, col, main_title = "Cluster Analysis") {
  g <- ggplot(data,
              aes(x = true_classes, y = cluster_fit$cluster,
                  fill = true_classes)) +
    geom_boxplot(fill = "white") +
    geom_jitter(aes(color = true_classes),
                alpha = 0.4) +
    scale_x_discrete(name = "Model") +
    scale_y_continuous(name = "Cluster") +
    ggtitle(main_title) +
    scale_color_manual(values = col) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 12, face = "bold"),
          axis.text.y = element_text(size = 12, face = "bold"),
          axis.title.x = element_text(size = 14, face="bold"),
          axis.title.y = element_text(size = 14, face="bold"),
          plot.title = element_text(size = 16, face = "bold"),
          legend.position="none",
          panel.background = element_rect(fill = "gray97",
                                          colour = "black",
                                          size = 0.5, linetype = "solid"))
  
  return(g)
}





## function to draw multi ggplots
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
### extample of usage: multiplot(g1, g2, g3, g4, g5, cols = 2)
