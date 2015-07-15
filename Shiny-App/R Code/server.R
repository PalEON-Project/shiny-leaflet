library(shiny)

# server.R
rm(list=ls()) #clear workspace

#require(ncdf)
require(ncdf4)
require(reshape) # reshape2?
require(ggplot2)
require(gridExtra)
require(maptools)
require(sp)
require(rgdal)
require(fields)
require(raster)
require(maps)
require(RColorBrewer)

source("plot.R")
source("set_domain.R")

library(maptools)

####################################################################################################################################
#We couldn't get the US Albers shapefile to show up when running the Shiny App.
#But we had the code below from Alex Dye for viewing Canada that he used when playing around with Shiny that we could make work 
library(maptools)

shinyServer(function(input,output){
  Canada<-readShapeSpatial("Canada.shp")
  #USA<-readShapeSpatial("locMap2.shp")
  
  output$MapPlot<-renderPlot({
    paste("you have selected",input$selectInput)
    
    plot(Canada,col=input$mapColor)
  })
})



###################################################################################################################################
#Code for making composition maps
#Chris Shapefile Code
##fileInput('inputdata','Input shapefile',accept=c('.shp','.dbf','.shx',".prj"), multiple=TRUE)
##plotOutput("SHPplot")

#load US Shapefile which was obtained from wiki page: https://paleon.geography.wisc.edu/doku.php/public_data;rasters
#usShp <-readShapeLines(("us_alb.shp"), proj4string=CRS('+init=epsg:3175'))
###usShp <- readShapeLines(("C:/Dropbox/Shiny/Shiny-App/Data/us_alb.shp"), proj4string=CRS('+init=epsg:3175'))
#usShp@data$id <- rownames(usShp@data)
#usFortified <- fortify(usShp, region='id')

region = t(as.matrix(raster("paleonDomain.tif")))
water = t(as.matrix(raster("water.tif")))

#region = t(as.matrix(raster("C:/Dropbox/Shiny/Shiny-App/Data/paleonDomain.tif")))
#water = t(as.matrix(raster("C:/Dropbox/Shiny/Shiny-App/Data/water.tif")))
## t()  manipulates matrix so plots correctly W-E and N-S in R

## region[region %in% c(2,3,5,6,11,12)] <- NA
water[water == 100] <- NA
mask = is.na(region)
maskWater = is.na(water)

## western data/results

finalNcdfName <- paste0('PLScomposition_western_0.3.nc')

ncdfPtr <- nc_open("C:/Dropbox/Shiny/Shiny-App/Data/composition_midwest_0.3.nc")

taxaNames <- names(ncdfPtr$var)#use this so I dont' have to refer back to the western/easterData.Rda files to get the taxa variable

#additional code from Chris to read from the netcdf file.
nCells <- ncdfPtr$dim[[1]]$len * ncdfPtr$dim[[2]]$len
nTaxa <- ncdfPtr$nvars
nSamples <- ncdfPtr$dim[[3]]$len

preds <- array(0, c(nCells, nTaxa, nSamples))
#dimnames(preds)[[2]] <- rep("name",19)# taxa$taxonName
for(p in 1:nTaxa) {
  preds[ , p, ] <- ncvar_get(ncdfPtr, taxaNames[p], c(1, 1, 1), c(-1, -1, -1))
}


attributes(preds)$dimnames[[2]] <- gsub("/", "ZZZ", taxaNames) 

pmWest <- apply(preds, c(1, 2), 'mean')
psdWest <- apply(preds, c(1, 2), 'sd')


# eastern data/results

finalNcdfName <- paste0('PLScomposition_eastern_0.3.nc')

ncdfPtr <- nc_open("C:/Dropbox/Shiny/Shiny-App/Data/composition_east_0.3.nc")
taxaNames <- names(ncdfPtr$var)#use this so I dont' have to refer back to the western/easterData.Rda files to get the taxa variable

#additional code from Chris to read from the netcdf file.
nCells <- ncdfPtr$dim[[1]]$len * ncdfPtr$dim[[2]]$len
nTaxa <- ncdfPtr$nvars
nSamples <- ncdfPtr$dim[[3]]$len

#test <- ncvar_get(ncdfPtr, "Oak", c(1, 1, 1), c(-1, -1, -1))

preds <- array(0, c(nCells, nTaxa, nSamples))
for(p in 1:nTaxa) {
  preds[ , p, ] <- ncvar_get(ncdfPtr, taxaNames[p], c(1, 1, 1), c(-1, -1, -1))
}

attributes(preds)$dimnames[[2]] <- gsub("/", "ZZZ", taxaNames) 

pmEast <- apply(preds, c(1, 2), 'mean')
psdEast <- apply(preds, c(1, 2), 'sd')

#attempt to put composition maps in shinyServer code
shinyServer(
  function(input, output) {
    pmFull <- matrix(NA, c(xRes*yRes), ncol(pmEast))
    dimnames(pmFull)[[2]] <- dimnames(pmEast)[[2]]
    fullTmp <- psdFull <- rawFull <- pmFull
    
    ids <- matrix(1:(xRes*yRes), nrow = xRes, ncol = yRes)
    eastSubset <- c(ids[easternDomainX, easternDomainY])
    westSubset <- c(ids[westernDomainX, westernDomainY])
    
    eastOnly <- dimnames(pmEast)[[2]]
    eastOnly <- eastOnly[!(eastOnly %in% dimnames(pmWest)[[2]])]
    
    pmFull[eastSubset, ] <- pmEast
    fullTmp[westSubset, dimnames(pmWest)[[2]]] <- pmWest
    fullTmp[!(region %in% c(6,12,11, 5, 2, 3)), ] <- NA
    pmFull[!is.na(fullTmp)] <- fullTmp[!is.na(fullTmp)]
    pmFull[region %in% c(5, 12), eastOnly] <- NA
    
    psdFull[eastSubset, ] <- psdEast
    fullTmp[westSubset, dimnames(psdWest)[[2]]] <- psdWest
    fullTmp[!(region %in% c(6,12,11, 5, 2, 3)), ] <- NA
    psdFull[!is.na(fullTmp)] <- fullTmp[!is.na(fullTmp)]
    psdFull[region %in% c(5, 12), eastOnly] <- NA
    
    #rawFull[westSubset, dimnames(rawWest)[[2]]] <- rawWest
    
    pmFull[mask, ] <- NA
    psdFull[mask, ] <- NA
    #rawFull[mask, ] <- NA
    
    #tmp <- pmFull
    # tmp<tmp[1:296, 180:1]
    #image.plot(1:296,1:180, matrix(tmp,296,180))
    
    coordFull <- expand.grid(X = xGrid, Y = rev(yGrid))
    
    output$map <- renderPlot({
      
      myshape<- input$inputdata
      if (is.null(myshape)) 
        return(NULL)       
      
      dir<-dirname(myshape[1,4])
      
      for ( i in 1:nrow(myshape)) {
        file.rename(myshape[i,4], paste0(dir,"/",myshape[i,1]))}
      
     
      getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
      shape<-readShapePoly(getshp)
      plot(shape)
      
      
      propBreaks = c(0, 0.01, 0.05, 0.10, 0.15, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1)
      
      figHgt = 16
      figWth = 22
      figHgtIndiv = 16*.5
      figWthIndiv = 22*.5
      
      make_veg_map(data = pmFull, breaks = propBreaks, coords = coordFull, legendName = 'fitted proportions', map_data = usFortified, facet = TRUE)
      
    })
    
  }
    )