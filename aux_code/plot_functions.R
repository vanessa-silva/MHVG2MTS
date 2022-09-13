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
acf_plot <- function(ts, y_title = "", x_title = "", main_title = "", linf = -1, lsup = -1) {
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
ccf_plot <- function(ts_a, ts_b, y_title = "", x_title = "", main_title = "", linf = -1, lsup = -1, flag = 0) {
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
