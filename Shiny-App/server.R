library(shiny)
require(ggplot2)
require(RColorBrewer)
library(gridExtra)
library(grid)

all_taxa <- readRDS('data/all_taxa.RDS')

load('data/shape.RData')

thresh <- function(data, lim) {
    data[data > lim] <- lim
    return(data)
}

####################################################################################################################################
#We couldn't get the US Albers shapefile to show up when running the Shiny App.
#But we had the code below from Alex Dye for viewing Canada that he used when playing around with Shiny that we could make work 

shinyServer(function(input,output){
  
  dataset <- reactive(function(){
    sub <- subset(all_taxa, taxon == input$taxon)
    sub$means <- thresh(sub$means, input$zlimit)
    sub
   })
  
  output$MapPlot<-renderPlot({
    
    mean_plot <- ggplot(dataset(), aes(x = x, y = y, fill = means)) + 
        geom_tile() +
            scale_fill_gradient2(na.value = NA, low = '#ffebaf', mid = '#98e600', high = '#267300', midpoint = input$zlimit/2) +
    coord_equal() + ggtitle("Posterior mean proportion") +
     theme_bw() + theme(axis.ticks = element_blank(), 
                               axis.text.y = element_blank(), 
                               axis.text.x = element_blank(),
                               axis.title.x = element_blank(),
                               axis.title.y = element_blank())
         

    sd_plot <- ggplot(dataset(), aes(x = x, y = y, fill = sds)) + 
      geom_tile() +
      scale_fill_continuous(na.value = NA, low = 'gray', high = 'red') +
      coord_equal() + ggtitle("Posterior standard deviation") +
      theme_bw() + theme(axis.ticks = element_blank(), 
                               axis.text.y = element_blank(), 
                               axis.text.x = element_blank(),
                               axis.title.x = element_blank(),
                               axis.title.y = element_blank()) 

    
    
    grid.arrange(mean_plot, sd_plot, ncol = 1)
    
  }, width = 900, height = 900)
})
