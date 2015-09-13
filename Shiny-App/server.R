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
                  map2 = NA,
                  map3 = NA,
                  map4 = NA)


#  This truncates the data values to some upper limit `lim`:
thresh <- function(data, lim) {
    if(class(data) == 'raster'){
      data.vals <- getValues(data)
      data.vals[data.vals > max(lim)] <- max(lim)
      data <- setValues(data, data.vals)
    } else {
      data[data > max(lim)] <- max(lim)
    }
  
    return(data)
}

breaks <- c(0, 0.01, 0.05, 0.10, 0.15, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1)
sd_breaks <- c(0, 0.01, 0.03, 0.05, 0.075, 0.10, 0.15, 0.2, 0.25)

max_sd <- max(sd_breaks)

#  Set the plotting colors 
mean_col <- rev(terrain.colors(length(breaks) - 1))
mean_col[1] <- terrain.colors(40)[39] # This whitens the lowest value marginally. 

sd_col <- rev(heat.colors(length(sd_breaks)-1))

mg0 <- c(0, 0, 0, 0)
mg1 <- c(-5, 0, 0, 0)
mg2 <- c(-12, 0, 0, 0)

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
    
    if(!input$continuous) {
        output$meansDiscrete <- cut(output$meansCut, breaks = breaks, include.lowest = TRUE)
        output$sdsDiscrete <- cut(output$sdsCut, breaks = breaks, include.lowest = TRUE)
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
    
    pal_ter <- colorNumeric(
      palette = rev(terrain.colors(40)),
      domain = getValues(dataset1()$meansCut),
      na.color=NA)
    
    map <- leaflet() %>% addProviderTiles("Esri.WorldImagery") %>%
        fitBounds(lng1=-98.6,lng2=-66.1,lat1=36.5,lat2=49.75) %>%
        addRasterImage(layerId = "ds1Mean",
                       dataset1()$meansCut, 
                       opacity = input$transparancy,
                       colors = pal_ter) %>%
        addLegend("bottomright", pal = pal_ter, 
                  values = values(dataset1()$meansCut),
                  title = "Proportion",
                  labFormat = labelFormat(),
                  opacity = 1
        )
    })
  
  output$MapPlot2<-renderLeaflet({
    
    pal_sd <- colorNumeric(
      palette = c('#fbdede', '#e50000'),
      domain = getValues(dataset1()$sdsCut),
      na.color=NA)
    
    map <- leaflet() %>% addProviderTiles("Esri.WorldImagery") %>%
      fitBounds(lng1=-98.6,lng2=-66.1,lat1=36.5,lat2=49.75) %>%
        addRasterImage(layerId = "ds1SD",
                       dataset1()$meansCut, 
                       opacity = input$transparancy,
                       colors = pal_sd) %>%
      addLegend("bottomright", pal = pal_sd, 
                values = values(dataset1()$sdsCut),
                title = "St. Dev.",
                labFormat = labelFormat(),
                opacity = 1
      )
    
  })
  
  output$MapPlot3<-renderLeaflet({
    
    pal_ter <- colorNumeric(
      palette = rev(terrain.colors(40)),
      domain = getValues(dataset2()$meansCut),
      na.color=NA)
    
    map <- leaflet() %>% addProviderTiles("Esri.WorldImagery") %>%
      fitBounds(lng1=-98.6,lng2=-66.1,lat1=36.5,lat2=49.75) %>%
      addRasterImage(layerId = "ds2Mean",
                     dataset2()$meansCut, 
                     opacity = input$transparancy,
                     colors = pal_ter)
    
  })

  output$MapPlot4<-renderLeaflet({
    
    pal_sd <- colorNumeric(
      palette = c('#fbdede', '#e50000'),
      domain = getValues(dataset2()$sdsCut),
      na.color=NA)
    
    map <- leaflet() %>% addProviderTiles("Esri.WorldImagery") %>%
      fitBounds(lng1=-98.6,lng2=-66.1,lat1=36.5,lat2=49.75) %>%
      addRasterImage(layerId = "ds2SD",
                     dataset2()$sds, 
                     opacity = input$transparancy,
                     color = pal_sd)
    
  })
  
  
  
  observe({

    pal_sd <- colorNumeric(
      palette = c('#fbdede', '#e50000'),
      domain = c(0,input$zlimit_sd),
      na.color=NA)
    
    pal_ter <- colorNumeric(
      palette = rev(terrain.colors(40)),
      domain = c(0,input$zlimit),
      na.color=NA)
        
    leafletProxy("MapPlot") %>%
      clearImages() %>%
      addRasterImage(layerId = "ds1Mean",
                     dataset1()$meansCut, 
                     opacity = input$transparancy,
                     colors = pal_ter)
    
    leafletProxy("MapPlot2") %>%
      clearImages() %>%
      addRasterImage(layerId = "ds1SD",
                     dataset1()$sdsCut, 
                     opacity = input$transparancy,
                     colors = pal_sd)
    
    leafletProxy("MapPlot3") %>%
      clearImages() %>%
      addRasterImage(layerId = "ds2Mean",
                     dataset2()$meansCut, 
                     opacity = input$transparancy,
                     colors = pal_ter)
    
    leafletProxy("MapPlot4") %>%
      clearImages() %>%
      addRasterImage(layerId = "ds2SD",
                     dataset2()$sdsCut, 
                     opacity = input$transparancy,
                     colors = pal_sd)
  
  })
      
})
