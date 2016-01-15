library(shiny)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(grid)
library(leaflet)
library(raster)

#  Load the taxon data for all taxa:
all_taxa <- readRDS('data/all_taxa_wm.RDS')

boundlist <- list(map1 = NA,
                  map2 = NA)

#  This truncates the data values to limits `lim`, that will come from the user:
thresh <- function(data, lim, disc = FALSE) {
    if(class(data) == 'raster' | class(data) == 'RasterLayer'){
      data.vals <- getValues(data)
      data.vals[data.vals > max(lim)] <- max(lim)
      data.vals[data.vals < min(lim)] <- NA
      
      if(disc == TRUE){
        # This discretizes the data into a set of bins.
        new.vals <- pretty(data.vals)[findInterval(data.vals, pretty(data.vals)) + 1]
        new.vals[data.vals == 0] <- 0
        data.vals <- new.vals
      }
      
      data <- setValues(data, data.vals)
    } else {
      data[data > max(lim)] <- max(lim)
      data[data < min(lim)] <- NA
      
      if(disc == TRUE){
        new.vals <- pretty(data)[findInterval(data, pretty(data)) + 1]
        new.vals[data == 0] <- 0
        data <- new.vals
      }
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
    # 4. Convert to a discrete scale (if desired)
    
    sub <- subset(all_taxa, taxon == input$taxon1)
    
    output <- list()
    
    # Raster for leaflet's pseudo-web Mercator:
    sub_raster <- raster(xmn=-10965655,xmx=-7236655,
                         ymn=4253944,ymx=6508944,
                         crs="+init=epsg:3857", resolution = 11000)
    # We are going to have:
    #  1. the raw values (sds & mean)
    #  2. the clipped, continuous values (sdsCont & meanCont)
    #  3. the clipped, discrete values   (sdsDisc & meanDisc)
    
    output$sds  <- setValues(sub_raster, unlist(sub$sds))
    output$mean <- setValues(sub_raster, unlist(sub$means))
    
    output$sdsCont  <- thresh(output$sds, input$zlimit_sd)
    output$meanCont <- thresh(output$mean, input$zlimit)
    
    output$meanDisc <- thresh(output$mean, input$zlimit, disc = TRUE)
    output$sdsDisc  <- thresh(output$sds, input$zlimit_sd, disc = TRUE)
    
    output$sdsDisc[is.na(output$meanDisc)] <- NA
    output$sdsCont[is.na(output$meanCont)] <- NA
    
    output
    
   })
  
  dataset2 <- reactive({
    #  Recut the second dataset:
    # 1. extract the taxon specific information
    # 2. Put the data into a raster for display with leaflet
    # 3. Clip the upper values to the user defined upper limit (with `thresh`).
    # 4. Convert to a discrete scale (if desired)
    
    sub_raster <- raster(xmn=-10965655,xmx=-7236655,
                         ymn=4253944,ymx=6508944,
                         crs="+init=epsg:3857", resolution = 11000)
    
    sub <- subset(all_taxa, taxon == input$taxon2)
    
    output <- list()
    
    # We are going to have:
    #  1. the raw values (sds & mean)
    #  2. the clipped, continuous values (sdsCont & meanCont)
    #  3. the clipped, discrete values   (sdsDisc & meanDisc)
    
    output$sds  <- setValues(sub_raster, unlist(sub$sds))
    output$mean <- setValues(sub_raster, unlist(sub$means))
    
    output$sdsCont  <- thresh(output$sds, input$zlimit_sd)
    output$meanCont <- thresh(output$mean, input$zlimit)
    
    output$meanDisc <- thresh(output$mean, input$zlimit, disc = TRUE)
    output$sdsDisc  <- thresh(output$sds, input$zlimit_sd, disc = TRUE)
    
    output$sdsDisc[is.na(output$meanDisc)] <- NA
    output$sdsCont[is.na(output$meanCont)] <- NA

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
      
      file.copy(filename, file)
    },
    contentType = "application/zip"
  )
  
  # Plotting out the upper panel:
  output$MapPlot1 <- renderLeaflet({
    # Renders the upper plot.

    if(input$continuous == TRUE) {
      if(input$sd_box_1 == TRUE) {
        # Is it the standard deviation or the proportions?
        plotvals <- dataset1()$sdsCont
        palette_in <- input$sdPalette
        title <- paste0("St. Dev.\n", input$taxon1)
      } else {
        plotvals <- dataset1()$meanCont
        palette_in <- input$rampPalette
        title <- paste0("Proportion\n", input$taxon1)
      }
      
      ranger <- range(getValues(plotvals), na.rm=TRUE)
      
      if(input$sd_box_1 == TRUE){
        ranger[2] <- max(ranger[2], input$zlimit_sd[2])
      } else {
        ranger[2] <- max(ranger[2], input$zlimit[2])
      }
      
      # Either way, we use a numeric palette:
      palette <- colorNumeric(palette = palette_in,
                             domain = ranger,
                             na.color=NA)
    } else {
      if(input$sd_box_1 == TRUE) {
        # Is it the standard deviation or the proportions?
        plotvals <- dataset1()$sdsDisc
        palette_in <- input$sdPalette
        title <- paste0("St. Dev.\n", input$taxon1)
      } else {
        plotvals <- dataset1()$meanDisc
        palette_in <- input$rampPalette
        title <- paste0("Proportion\n", input$taxon1)
      }
      
      palette <- colorFactor(palette = input$rampPalette,
                             domain = levels(factor(getValues(plotvals))),
                             na.color=NA)
    }

    #  Laying out the map (without pipe notation):
    map <- addProviderTiles(leaflet(), 
                            input$baseTile)
    
    map <- addRasterImage(map, 
                          layerId = "UpperPlot",
                          plotvals, 
                          opacity = input$opacity/100,
                          colors = palette,
                          project = FALSE)
    
    if(input$labelTile){
      #  This adds the labels above the raster layer:
      map <- addProviderTiles(map, "Stamen.TonerLabels")
    }
    
    map <- addLegend(map, "bottomright", pal = palette, 
                   values = values(plotvals),
                   title = title,
                   labFormat = labelFormat(),
                   opacity = 1)
  
      
    setView(map, lat = 43, lng = -81, zoom = 4)
    })
  
  # Plotting out the lower panel:
  output$MapPlot2 <- renderLeaflet({
    
    if(input$continuous == TRUE) {
      # If this is all plotted on a continuous scale:
      if(input$sd_box_2 == TRUE) {
        # Is it the standard deviation or the proportions?
        plotvals <- dataset2()$sdsCont
        palette_in <- input$sdPalette
        title <- paste0("St. Dev.\n", input$taxon2)
      } else {
        plotvals <- dataset2()$meanCont
        palette_in <- input$rampPalette
        title <- paste0("Proportion\n", input$taxon2)
      }
      
      ranger <- range(getValues(plotvals), na.rm=TRUE)
      
      if(input$sd_box_2 == TRUE){
        ranger[2] <- max(ranger[2], input$zlimit_sd[2])
      } else {
        ranger[2] <- max(ranger[2], input$zlimit[2])
      }

            # Either way, we use a numeric palette:
      palette <- colorNumeric(palette = palette_in,
                              domain = ranger,
                              na.color=NA)
    } else {
      # If this is plotted on a discrete scale.
      if(input$sd_box_2 == TRUE) {
        # Is it the standard deviation or the proportions?
        plotvals <- dataset2()$sdsDisc
        title <- paste0("St. Dev.\n", input$taxon2)
        palette_in <- input$sdPalette
      } else {
        plotvals <- dataset2()$meanDisc
        title <- paste0("Proportion\n", input$taxon2)
        palette_in <- input$rampPalette
      }
      
      palette <- colorFactor(palette = palette_in,
                             domain = factor(getValues(plotvals)),
                             na.color=NA)
    }
    
    #  Laying out the map (without pipe notation):
    map <- addProviderTiles(leaflet(),
                            input$baseTile)
                   
    map <- addRasterImage(map, 
                          layerId = "LowerPlot",
                          plotvals, 
                          opacity = input$opacity/100,
                          colors = palette,
                          project = FALSE)
    
    if(input$labelTile){
      map <- addProviderTiles(map, "Stamen.TonerLabels")
    }
    
    map <- addLegend(map, "bottomright", pal = palette, 
                      values = values(plotvals),
                      title = title,
                      labFormat = labelFormat(),
                      opacity = 1)
    
    setView(map, lat = 43, lng = -81, zoom = 4)
  })
  
  runner <- 1
  
  observe({
    leafletProxy("MapPlot2") %>%
      setView(lng = input$MapPlot1_center$lng,
              lat = input$MapPlot1_center$lat,
              zoom = input$MapPlot1_zoom)
    })
  
#  Ideally I would observe both panels, but if I mess around
#  too much I get stuck in an infinite loop of re-plotting.
#
#  observe({
#    leafletProxy("MapPlot1") %>%
#      setView(lng = input$MapPlot2_center$lng,
#              lat = input$MapPlot2_center$lat,
#              zoom = input$MapPlot2_zoom) })
})