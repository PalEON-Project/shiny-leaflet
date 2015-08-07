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
sd_breaks <- c(0, 0.01, 0.03, 0.05, 0.075, 0.10, 0.15, 0.2, 0.25)
max_sd <- max(sd_breaks)
col <- rev(terrain.colors(length(breaks) - 1))
col[1] <- terrain.colors(40)[39]

sd_col <- rev(heat.colors(length(sd_breaks)-1))


# avoid having state boundaries cut off on edges
xbuffer = c(-35000, 10000)
ybuffer = c(0, 10000)

mg0 <- c(0, 0, 0, 0)
mg1 <- c(-5, 0, 0, 0)
mg2 <- c(-12, 0, 0, 0)
####################################################################################################################################
#We couldn't get the US Albers shapefile to show up when running the Shiny App.
#But we had the code below from Alex Dye for viewing Canada that he used when playing around with Shiny that we could make work 

shinyServer(function(input,output){
  
  dataset1 <- reactive(function(){
    sub <- subset(all_taxa, taxon == input$taxon1)
    sub$sds <- thresh(sub$sds, max_sd)
    sub$meansCut <- thresh(sub$means, input$zlimit)
    sub$sdsCut <- thresh(sub$sds, input$zlimit_sd)
    if(!input$continuous) {
        sub$meansDiscrete <- cut(sub$meansCut, breaks = breaks, include.lowest = TRUE,labels = FALSE)
        sub$sdsDiscrete <- cut(sub$sdsCut, breaks = breaks, include.lowest = TRUE,labels = FALSE)
    }
    sub
   })
  dataset2 <- reactive(function(){
    if(input$taxon2 != "None") {
        sub <- subset(all_taxa, taxon == input$taxon2)
        sub$sds <- thresh(sub$sds, max_sd)
        sub$meansCut <- thresh(sub$means, input$zlimit)
        sub$sdsCut <- thresh(sub$sds, input$zlimit_sd)
        if(!input$continuous) {
            sub$meansDiscrete <- cut(sub$meansCut, breaks = breaks, include.lowest = TRUE,labels = FALSE)
            sub$sdsDiscrete <- cut(sub$sdsCut, breaks = breaks, include.lowest = TRUE,labels = FALSE)
        }
        sub
    } else NULL
   })
  
  output$MapPlot<-renderPlot({
     if(input$taxon2 == "None") mg <- mg0 else mg <- mg1
     tmp <- cbind(breaks[1:(length(breaks)-1)], breaks[2:length(breaks)])
     tmp[length(unique(dataset1()$meansDiscrete)), 2] <- max(dataset1()$means)
     breaklabels <- apply(tmp, 1,  function(r) { sprintf("%0.2f - %0.2f", r[1], r[2]) })
    if(!input$continuous) {
        mean_plot <- ggplot(dataset1(), aes(x = x, y = y, fill = factor(meansDiscrete))) + scale_fill_manual(values = col, labels = breaklabels, guide = 'legend', name = 'proportion') } else {
#        tmpcol <- rev(terrain.colors(16)); tmpcol[1] <- terrain.colors(40)[39]
        mean_plot <- ggplot(dataset1(), aes(x = x, y = y, fill = meansCut)) +  
#            scale_fill_gradient2(na.value = NA, low = '#ffebaf', mid = '#98e600', high = '#267300', midpoint = input$zlimit/2, name = 'proportion')
            scale_fill_gradientn(na.value = NA, colours = rev(terrain.colors(15)), name = 'proportion',
                                   guide = 'colourbar')
    }
    mean_plot <- mean_plot + geom_tile() + 
    coord_equal() + ggtitle(paste(input$taxon1, ": Posterior mean proportion")) +
     theme_bw() +  theme(axis.ticks = element_blank(), 
                              axis.text.y = element_blank(), 
                               axis.text.x = element_blank(),
                               axis.title.x = element_blank(),
                               axis.title.y = element_blank(),
                               plot.margin = unit(mg, "cm"))
     mean_plot1 <- mean_plot + geom_path(data = usFortified, aes(x= long, y = lat, group = group, fill = NULL, colour = NULL)) +
         scale_x_continuous(limits = xbuffer + c(min(dataset1()$x, na.rm = TRUE), max(dataset1()$x, na.rm = TRUE))) +
      scale_y_continuous(limits = ybuffer + c(min(dataset1()$y, na.rm = TRUE), max(dataset1()$y, na.rm = TRUE)))
          

     tmp <- cbind(sd_breaks[1:(length(sd_breaks)-1)], breaks[2:length(sd_breaks)])
     tmp[length(unique(dataset1()$sdsDiscrete)), 2] <- max(dataset1()$sds)
     sd_breaklabels <- apply(tmp, 1,  function(r) { sprintf("%0.2f - %0.2f", r[1], r[2]) })

    if(!input$continuous) {
        sd_plot <- ggplot(dataset1(), aes(x = x, y = y, fill = factor(sdsDiscrete))) + scale_fill_manual(values = sd_col, labels = breaklabels, guide = 'legend', name = '        sd    ') } else {
#        tmpcol <- rev(terrain.colors(16)); tmpcol[1] <- terrain.colors(40)[39]
        sd_plot <- ggplot(dataset1(), aes(x = x, y = y, fill = sdsCut)) +  
#            scale_fill_gradient2(na.value = NA, low = '#ffebaf', mid = '#98e600', high = '#267300', midpoint = input$zlimit/2, name = 'proportion')
            scale_fill_gradientn(na.value = NA, colours = rev(heat.colors(15)), name = '         sd    ',
                                   guide = 'colourbar')
    }
     
      sd_plot <- sd_plot + geom_tile() +
      coord_equal() + ggtitle(paste(input$taxon1, ": Posterior standard deviation")) +
          theme_bw() + theme(axis.ticks = element_blank(), 
                               axis.text.y = element_blank(), 
                               axis.text.x = element_blank(),
                               axis.title.x = element_blank(),
                               axis.title.y = element_blank(),
                               plot.margin = unit(mg, "cm"))
     sd_plot1 <- sd_plot + geom_path(data = usFortified, aes(x= long, y = lat, group = group, fill = NULL, colour = NULL)) +
         scale_x_continuous(limits = xbuffer + c(min(dataset1()$x, na.rm = TRUE), max(dataset1()$x, na.rm = TRUE))) +
      scale_y_continuous(limits = ybuffer + c(min(dataset1()$y, na.rm = TRUE), max(dataset1()$y, na.rm = TRUE)))

    if(input$taxon2 != "None") {
        # some repeated code here; could be cleaned up
        tmp <- cbind(breaks[1:(length(breaks)-1)], breaks[2:length(breaks)])
        tmp[length(unique(dataset2()$meansDiscrete)), 2] <- max(dataset2()$means)
        breaklabels <- apply(tmp, 1,  function(r) { sprintf("%0.2f - %0.2f", r[1], r[2]) })
        if(!input$continuous) {
            mean_plot <- ggplot(dataset2(), aes(x = x, y = y, fill = factor(meansDiscrete))) + scale_fill_manual(values = col, labels = breaklabels, guide = 'legend', name = 'proportion') } else {
                                        #        tmpcol <- rev(terrain.colors(16)); tmpcol[1] <- terrain.colors(40)[39]
                                                                                                                                                                                                  mean_plot <- ggplot(dataset2(), aes(x = x, y = y, fill = meansCut)) +  
                                        #            scale_fill_gradient2(na.value = NA, low = '#ffebaf', mid = '#98e600', high = '#267300', midpoint = input$zlimit/2, name = 'proportion')
                                                                                                                                                                                                      scale_fill_gradientn(na.value = NA, colours = rev(terrain.colors(15)), name = 'proportion',
                                                                                                                                                                                                                           guide = 'colourbar')
                                                                                                                                                                                              }
        mean_plot <- mean_plot + geom_tile() + 
            coord_equal() + ggtitle(paste(input$taxon2, ": Posterior mean proportion")) +
                theme_bw() +  theme(axis.ticks = element_blank(), 
                                    axis.text.y = element_blank(), 
                                    axis.text.x = element_blank(),
                                    axis.title.x = element_blank(),
                                    axis.title.y = element_blank(),
                               plot.margin = unit(mg+mg2, "cm"))
        mean_plot2 <- mean_plot + geom_path(data = usFortified, aes(x= long, y = lat, group = group, fill = NULL, colour = NULL)) +
            scale_x_continuous(limits = xbuffer + c(min(dataset2()$x, na.rm = TRUE), max(dataset2()$x, na.rm = TRUE))) +
                scale_y_continuous(limits = ybuffer + c(min(dataset2()$y, na.rm = TRUE), max(dataset2()$y, na.rm = TRUE)))
        
        
        tmp <- cbind(sd_breaks[1:(length(sd_breaks)-1)], breaks[2:length(sd_breaks)])
        tmp[length(unique(dataset2()$sdsDiscrete)), 2] <- max(dataset2()$sds)
        sd_breaklabels <- apply(tmp, 1,  function(r) { sprintf("%0.2f - %0.2f", r[1], r[2]) })
        
        if(!input$continuous) {
            sd_plot <- ggplot(dataset2(), aes(x = x, y = y, fill = factor(sdsDiscrete))) + scale_fill_manual(values = sd_col, labels = breaklabels, guide = 'legend', name = '        sd    ') } else {
                                        #        tmpcol <- rev(terrain.colors(16)); tmpcol[1] <- terrain.colors(40)[39]
                                                                                                                                                                                                     sd_plot <- ggplot(dataset2(), aes(x = x, y = y, fill = sdsCut)) +  
                                        #            scale_fill_gradient2(na.value = NA, low = '#ffebaf', mid = '#98e600', high = '#267300', midpoint = input$zlimit/2, name = 'proportion')
                                                                                                                                                                                                         scale_fill_gradientn(na.value = NA, colours = rev(heat.colors(15)), name = '         sd    ',
                                                                                                                                                                                                                              guide = 'colourbar')
                                                                                                                                                                                                 }
        
        sd_plot <- sd_plot + geom_tile() +
            coord_equal() + ggtitle(paste(input$taxon2, ": Posterior standard deviation")) +
                theme_bw() + theme(axis.ticks = element_blank(), 
                                   axis.text.y = element_blank(), 
                                   axis.text.x = element_blank(),
                                   axis.title.x = element_blank(),
                                   axis.title.y = element_blank(),
                               plot.margin = unit(mg+mg2, "cm"))
        sd_plot2 <- sd_plot + geom_path(data = usFortified, aes(x= long, y = lat, group = group, fill = NULL, colour = NULL)) +
            scale_x_continuous(limits = xbuffer + c(min(dataset2()$x, na.rm = TRUE), max(dataset2()$x, na.rm = TRUE))) +
                scale_y_continuous(limits = ybuffer + c(min(dataset2()$y, na.rm = TRUE), max(dataset2()$y, na.rm = TRUE)))
        
        
        grid.arrange(mean_plot1, sd_plot1, mean_plot2, sd_plot2, ncol = 2)
    } else grid.arrange(mean_plot1, sd_plot1, ncol = 1)
    
  }, width = 700, height = 700)
})
