#tsGraphData <- function()
#top10graph



topNPayerdf <- function(inputdf,orderByCol = 3,dec = TRUE,n = 10){
  # 
  # top_10_payers = head(county.ts.tab2()[ order(county.ts.tab2()[[3]], decreasing = TRUE),], 10)[[3]]
  # labels_payers = head(county.ts.tab2()[ order(county.ts.tab2()[[3]], decreasing = TRUE),], 10)[[1]]
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



