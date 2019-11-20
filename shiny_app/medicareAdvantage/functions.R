#tsGraphData <- function()
#top10graph



topNPayerdf <- function(inputdf,orderByCol = 3,dec = TRUE,n = 10){
  topN = head(inputdf[ order(inputdf[[orderByCol]], decreasing = dec),], n)
  df <- data.frame(top.payers=topN[[3]],
                   payer.labels = topN[[1]])
  return(df)
}

topNPayerPlot <- function(df,plotTitle){
  
  p <- ggplot(data=df, aes(x=payer.labels, y=top.payers, fill=top.payers)) +
    geom_bar(stat="identity", width = 0.60) +
    ggtitle(plotTitle) +
    scale_x_discrete(label = function(x) stringr::str_trunc(x, 12)) +  # truncate data names to 12 characters
    scale_y_continuous(labels=comma) +            # add commas to value labels
    theme_minimal() +                             # remove grey background
    scale_fill_viridis() +                        # add viridis color palette
    theme(axis.text.y = element_text(hjust=0),    # left justify labels
          axis.title.x=element_blank(),           # remove x title
          axis.title.y=element_blank(),           # remove y title
          legend.position="none",                 # remove legend
          plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust = 0.5)) +
    coord_flip()
  return(p)
}




meltDF2TS <- function(df,mid="Parent_Organization",originDate = "1899-12-30"){
  meltDF <- melt(df, id=mid)
  meltDF$variable <- as.Date( as.numeric (as.character(meltDF$variable) ),origin=originDate)
  return(meltDF)
}

tsPlot <- function(df, Title = "Time Series Analysis"){
  p <-ggplot(df, aes(variable, value, group = Parent_Organization, color = str_trunc(Parent_Organization, 12, "right"))) +
        geom_line() +
        geom_point() +
        ggtitle("Time Series Analysis") +
        theme_minimal() +
        scale_y_continuous(labels=comma) +
        scale_colour_viridis_d() +
        guides(col = guide_legend(nrow = 3)) +
        theme(legend.position="bottom",
            legend.title=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust = 0.5))
  
  return(p)
  
}