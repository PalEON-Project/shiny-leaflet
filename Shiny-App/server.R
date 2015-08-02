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

breaks <- c(0, 0.01, 0.05, 0.10, 0.15, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1)
col <- rev(terrain.colors(length(breaks) - 1))
col[1] <- terrain.colors(40)[39]

####################################################################################################################################
#We couldn't get the US Albers shapefile to show up when running the Shiny App.
#But we had the code below from Alex Dye for viewing Canada that he used when playing around with Shiny that we could make work 

shinyServer(function(input,output){
  
  dataset <- reactive(function(){
    sub <- subset(all_taxa, taxon == input$taxon)
    sub$meansCut <- thresh(sub$means, input$zlimit)
    if(!input$continuous)
        sub$meansDiscrete <- cut(sub$meansCut, breaks = breaks, include.lowest = TRUE,labels = FALSE)
    sub
   })
  
  output$MapPlot<-renderPlot({
     tmp <- cbind(breaks[1:(length(breaks)-1)], breaks[2:length(breaks)])
     tmp[length(unique(dataset()$meansDiscrete)), 2] <- max(dataset()$means)
     breaklabels <- apply(tmp, 1,  function(r) { sprintf("%0.2f - %0.2f", r[1], r[2]) })
    if(!input$continuous) {
        mean_plot <- ggplot(dataset(), aes(x = x, y = y, fill = factor(meansDiscrete))) + scale_fill_manual(values = col, labels = breaklabels, guide = 'legend', name = 'proportion') } else {
#        tmpcol <- rev(terrain.colors(16)); tmpcol[1] <- terrain.colors(40)[39]
        mean_plot <- ggplot(dataset(), aes(x = x, y = y, fill = meansCut)) +  
#            scale_fill_gradient2(na.value = NA, low = '#ffebaf', mid = '#98e600', high = '#267300', midpoint = input$zlimit/2, name = 'proportion')
            scale_fill_gradientn(na.value = NA, colours = rev(terrain.colors(15)), name = 'proportion',
                                   guide = 'colourbar')
    }
    mean_plot <- mean_plot + geom_tile() + 
    coord_equal() + ggtitle("Posterior mean proportion") +
     theme_bw() +  theme(axis.ticks = element_blank(), 
                              axis.text.y = element_blank(), 
                               axis.text.x = element_blank(),
                               axis.title.x = element_blank(),
                               axis.title.y = element_blank())
     mean_plot <- mean_plot + geom_path(data = usFortified, aes(x= long, y = lat, group = group, fill = NULL, colour = NULL)) +
         scale_x_continuous(limits = c(min(dataset()$x, na.rm = TRUE), max(dataset()$x, na.rm = TRUE))) +
      scale_y_continuous(limits = c(min(dataset()$y, na.rm = TRUE), max(dataset()$y, na.rm = TRUE)))
          

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
