library(shiny)
require(ggplot2)
require(RColorBrewer)
library(gridExtra)

all_taxa <- readRDS('data/all_taxa.RDS')

####################################################################################################################################
#We couldn't get the US Albers shapefile to show up when running the Shiny App.
#But we had the code below from Alex Dye for viewing Canada that he used when playing around with Shiny that we could make work 

shinyServer(function(input,output){
  
  dataset <- reactive(function(){
    subset(all_taxa, taxon == input$taxon)
   })
  
  output$MapPlot<-renderPlot({
    
    mean_plot <- ggplot(dataset(), aes(x = x, y = y, fill = means)) + 
      geom_tile() +
      scale_fill_gradient2(na.value = NA, low = '#ffebaf', mid = '#98e600', high = '#267300', midpoint = 0.15) +
      coord_equal() +
      theme_bw()
    sd_plot <- ggplot(dataset(), aes(x = x, y = y, fill = sds)) + 
      geom_tile() +
      scale_fill_continuous(na.value = NA, low = 'gray', high = 'red') +
      coord_equal() +
      theme_bw()
    
    grid.arrange(mean_plot, sd_plot, ncol = 1)
    
  })
})
