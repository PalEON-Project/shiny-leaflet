library(shiny)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(grid)
library(leaflet)
library(raster)

#  Load the taxon data for all taxa:
all_taxa <- readRDS('Data/all_taxa_ll.RDS')

boundlist <- list(map1 = NA,
                  map2 = NA)


#  This truncates the data values to some upper limit `lim`:
thresh <- function(data, lim) {
    if(class(data) == 'raster'){
      data.vals <- getValues(data)
      data.vals[data.vals > max(lim)] <- max(lim)
      data.vals[data.vals < min(lim)] <- NA
      data <- setValues(data, data.vals)
    } else {
      data[data > max(lim)] <- max(lim)
      data[data < min(lim)] <- NA
    }
  
    return(data)
}

# Actual server side code:

shinyServer(function(input,output){

  dataset1 <- reactive({
    #  Recut the first dataset. 
    # 1. extract the taxon specific information
    # 2. Put the data into a raster for display with leaflet
    # 3. Clip the upper values to the user defined upper limit (with `thresh`).
    # 4. Convert to a continuous scale (if desired)
    
    sub <- subset(all_taxa, taxon == input$taxon1)
    
    output <- list()
    
    sub_raster <- raster(xmn=-98.6,xmx=-66.1,ymn=36.5,ymx=49.75,
                         crs="+init=epsg:4326", resolution = 0.0833333)
    
    output$sds  <- setValues(sub_raster, unlist(sub$sds))
    output$mean <- setValues(sub_raster, unlist(sub$means))
    
    output$meansCut <- thresh(output$mean, input$zlimit)
    output$sdsCut   <- thresh(output$sds, input$zlimit_sd)
    
    output$sdsCut[is.na(output$meansCut)] <- NA
    
    if(!input$continuous) {
        output$meansDiscrete <- cut(output$meansCut, breaks = pretty(input$zlimit), include.lowest = TRUE)
        output$sdsDiscrete <- cut(output$sdsCut, breaks = pretty(input$zlimit), include.lowest = TRUE)
    }
    
    output
   })
  
  dataset2 <- reactive({
    #  Recut the second dataset:
    # 0. IF the user has chosen a second dataset. . . 
    # 1. extract the taxon specific information
    # 2. Put the data into a raster for display with leaflet
    # 3. Clip the upper values to the user defined upper limit (with `thresh`).
    # 4. Convert to a continuous scale (if desired)
    
    sub_raster <- raster(xmn=-98.6,xmx=-66.1,ymn=36.5,ymx=49.75,
                         crs="+init=epsg:4326", resolution = 0.0833333)
    
    if(input$taxon2 != "None") {
      
      sub <- subset(all_taxa, taxon == input$taxon2)
      
      output <- list()
      
      output$sds  <- setValues(sub_raster, unlist(sub$sds))
      output$mean <- setValues(sub_raster, unlist(sub$means))
      
      output$meansCut <- thresh(output$mean, input$zlimit)
      output$sdsCut   <- thresh(output$sds, input$zlimit_sd)
      
      if(!input$continuous) {
        output$meansDiscrete <- cut(output$meansCut, breaks = breaks, include.lowest = TRUE)
        output$sdsDiscrete <- cut(output$sdsCut, breaks = breaks, include.lowest = TRUE)
      }
      output
    } else {
      output <- list(mean = sub_raster,
                     sds = sub_raster,
                     meansCut = sub_raster,
                     sdsCut = sub_raster)
    }
                     
  })
  
  output$MapPlot<-renderLeaflet({
    
    # Renders the upper plot.

    if(!input$sd_box_1){
      # If a species is selected, but its SD is not chosen for display:
      plotvals <- dataset1()$meansCut
      title <- paste0("Proportion\n", input$taxon1)
      
      # Colors are pulled from the range of the
      # input data.
      palette <- colorNumeric(palette = input$rampPalette,
                               domain = getValues(dataset1()$meansCut),
                               na.color=NA)
      
    } else {
      
      plotvals <- dataset1()$sdsCut
      title <- paste0("St. Dev\n", input$taxon1)
      
      palette <- colorNumeric(palette = input$sdPalette,
                               domain = getValues(dataset1()$sdsCut),
                               na.color=NA)
      
    }
    
    if(input$labelTile){
      # If the user clicked 
      map <- leaflet() %>% addProviderTiles(input$baseTile) %>%
        fitBounds(lng1=-98.6,lng2=-66.1,lat1=36.5,lat2=49.75) %>%
        addRasterImage(layerId = "LowerPlot",
                       plotvals, 
                       opacity = input$opacity/100,
                       colors = palette) %>%
        addProviderTiles("Stamen.TonerLabels") %>%
      addLegend("bottomright", pal = palette, 
                values = values(plotvals),
                title = title,
                labFormat = labelFormat(),
                opacity = 1)
    } else {
      map <- leaflet() %>% addProviderTiles(input$baseTile) %>%
        fitBounds(lng1=-98.6,lng2=-66.1,lat1=36.5,lat2=49.75) %>%
        addRasterImage(layerId = "LowerPlot",
                       plotvals, 
                       opacity = input$opacity/100,
                       colors = palette) %>%         
        addLegend("bottomright", pal = palette, 
                  values = values(plotvals),
                  title = title,
                  labFormat = labelFormat(),
                  opacity = 1)
    }
    
    })
  
  output$MapPlot2<-renderLeaflet({
    
    if(!input$sd_box_2){
      # If a species is selected, but its SD is not chosen for display:
      plotvals <- dataset2()$meansCut
      title <- paste0("Propoportion\n",
                      input$taxon2)
      # Colors are pulled from the range of 
      palette <- colorNumeric(palette = input$rampPalette,
                              domain = getValues(dataset2()$meansCut),
                              na.color=NA)
      
      #  plot_legend <-   
    } else {
      
      plotvals <- dataset2()$sdsCut
      title <- paste0("St. Dev\n",
                      input$taxon2)
      
      palette <- colorNumeric(palette = input$sdPalette,
                              domain = getValues(dataset2()$sdsCut),
                              na.color=NA)
      
    }
    
    if(input$labelTile){
      # If the user clicked 
      map <- leaflet() %>% addProviderTiles(input$baseTile) %>%
        fitBounds(lng1=-98.6,lng2=-66.1,lat1=36.5,lat2=49.75) %>%
          addRasterImage(layerId = "LowerPlot",
                         plotvals, 
                         opacity = input$opacity/100,
                         colors = palette) %>%
        addProviderTiles("Stamen.TonerLabels") %>%
        addLegend("bottomright", pal = palette, 
          values = values(plotvals),
          title = title,
          labFormat = labelFormat(),
          opacity = 1)
    } else {
      map <- leaflet() %>% addProviderTiles(input$baseTile) %>%
        fitBounds(lng1=-98.6,lng2=-66.1,lat1=36.5,lat2=49.75) %>%
        addRasterImage(layerId = "LowerPlot",
                       plotvals, 
                       opacity = input$opacity/100,
                       colors = palette) %>%         
        addLegend("bottomright", pal = palette, 
                  values = values(plotvals),
                  title = title,
                  labFormat = labelFormat(),
                  opacity = 1)
    }
    
    
  })

})
