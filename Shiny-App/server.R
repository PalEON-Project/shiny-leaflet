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

#  This truncates the data values to limits `lim`, that will come from the user:
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
    # 1. extract the taxon specific information
    # 2. Put the data into a raster for display with leaflet
    # 3. Clip the upper values to the user defined upper limit (with `thresh`).
    # 4. Convert to a continuous scale (if desired)
    
    sub_raster <- raster(xmn=-98.6,xmx=-66.1,ymn=36.5,ymx=49.75,
                         crs="+init=epsg:4326", resolution = 0.0833333)
    
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

  })
  
  output$downloadData <- downloadHandler(

    filename = function(){
      paste0(ifelse(input$dlPanel == 'One', input$taxon1, input$taxon2), 
             '_', input$FileType, '.zip')
    },
    content = function(file) {
      filename <- paste0('data/zips/', input$FileType, '/',
                         ifelse(input$dlPanel == 'One', input$taxon1, input$taxon2), 
                         '_', input$FileType, '.zip')
      cat(filename, filename %in% list.files(paste0('data/zips/', input$FileType)))
      
      file.copy(filename, file)
    },
    contentType = "application/zip"
  )
  
  # Plotting out the upper panel:
  output$MapPlot1 <- renderLeaflet({
    
    # Renders the upper plot.

    if(!input$sd_box_1){
      # If a species is selected, but its SD is not chosen for display:
      plotvals <- dataset1()$meansCut
      title <- paste0("Proportion\n", input$taxon1)
      
      # Colors are pulled from the range of the input data.
      palette <- colorNumeric(palette = input$rampPalette,
                               domain = getValues(dataset1()$meansCut),
                               na.color=NA)
      
    } else {
      
      #  If the sd box is checked:
      plotvals <- dataset1()$sdsCut
      title <- paste0("St. Dev\n", input$taxon1)
      
      palette <- colorNumeric(palette = input$sdPalette,
                               domain = getValues(dataset1()$sdsCut),
                               na.color=NA)
      
    }
    
    #  Laying out the map (without pipe notation):
    map <- addProviderTiles(leaflet(), 
                            input$baseTile)
    
    map <- addRasterImage(map, 
                          layerId = "UpperPlot",
                          plotvals, 
                          opacity = input$opacity/100,
                          colors = palette)
    
    if(input$labelTile){
      #  This adds the labels above the raster layer:
      map <- addProviderTiles(map, "Stamen.TonerLabels")
    }
    
    map <- addLegend(map, "bottomright", pal = palette, 
                     values = values(plotvals),
                     title = title,
                     labFormat = labelFormat(),
                     opacity = 1)
    
    setView(map, lat = 43, lng = -81, zoom = 5)
    })
  
  # Plotting out the lower panel:
  output$MapPlot2 <- renderLeaflet({
    
    if(!input$sd_box_2){
      # As above. If a species is selected, but its SD is not chosen for display:
      plotvals <- dataset2()$meansCut
      title <- paste0("Propoportion\n",
                      input$taxon2)
      # Colors are pulled from the range of 
      palette <- colorNumeric(palette = input$rampPalette,
                              domain = getValues(dataset2()$meansCut),
                              na.color=NA)
    } else {
      
      plotvals <- dataset2()$sdsCut
      title <- paste0("St. Dev\n",
                      input$taxon2)
      
      palette <- colorNumeric(palette = input$sdPalette,
                              domain = getValues(dataset2()$sdsCut),
                              na.color=NA)
    }
    
    #  Laying out the map (without pipe notation):
    map <- addProviderTiles(leaflet(),
                            input$baseTile)
                   
    map <- addRasterImage(map, 
                          layerId = "LowerPlot",
                          plotvals, 
                          opacity = input$opacity/100,
                          colors = palette)
    
    if(input$labelTile){
      map <- addProviderTiles(map, "Stamen.TonerLabels")
    }
    
    map <- addLegend(map, "bottomright", pal = palette, 
                      values = values(plotvals),
                      title = title,
                      labFormat = labelFormat(),
                      opacity = 1)
    
    setView(map, lat = 43, lng = -81, zoom = 5)
  })
  
  observe({
    leafletProxy("MapPlot2") %>%
      setView(lng = input$MapPlot1_center$lng,
              lat = input$MapPlot1_center$lat,
              zoom = input$MapPlot1_zoom) })
  
  
})