#  This is the core function that prepares data for fast(ish) loading in the shiny app.
#  Written by: Simon Goring

library(raster)
library(ncdf4)
library(rgdal)
library(rgeos)

admin <- readOGR('../../../Maps/ne_10m_admin_1_states_provinces.shp', 'ne_10m_admin_1_states_provinces')
lakes <- readOGR('../../../Maps/NaturalEarth/ne_10m_lakes/ne_10m_lakes.shp', 'ne_10m_lakes')

admin <- admin[admin@data$geonunit %in% c('Canada', 'United States of America'),]

lakes_admin <- gIntersection(lakes, admin)

admin <- spTransform(admin, CRS("+init=epsg:3175"))
lakes_tr <- spTransform(lakes_admin, CRS("+init=epsg:3175"))

domain <- nc_open('Shiny-App/Data/composition_v0.3.nc', write = FALSE)

taxa <- unique(names(domain$var))

comp_grid <- expand.grid(x = ncvar_get(domain, 'x'), y = ncvar_get(domain, 'y'))

taxon_to_df <- function(taxon){

  # Calculate means & SDs for each taxon and each domain:
  mean_taxa <- as.vector(apply(ncvar_get(domain, taxon, c(1,1,1), c(-1, -1, -1)), c(1,2), mean))
  sd_taxa   <- as.vector(apply(ncvar_get(domain, taxon, c(1,1,1), c(-1, -1, -1)), c(1,2), sd))

  taxa_df <- data.frame(comp_grid,
                        taxon = taxon,
                        means = mean_taxa,
                        sds   = sd_taxa,
                        stringsAsFactors = FALSE)
  
  # To transform to the lat/long projection:
  # 1.  Turn it into a SpatialPointsDataFrame & rasterize
  # 2.  Reproject the raster to albers
  # 3. create a new data.frame with the Albers projection.
  
  coordinates(taxa_df) <- ~ x + y
  
  empty_ab <- raster(xmn=-71000,xmx=2297000,ymn=58000,ymx=1498000,
                     crs="+init=epsg:3175", resolution = 8000)
  
  empty_ll <- raster(xmn=-98.6,xmx=-66.1,ymn=36.5,ymx=49.75,
                     crs="+init=epsg:4326", resolution = 0.0833333)
  
  mean_rast <- rasterize(taxa_df, y = empty_ab, field = 'means', fun=mean, na.rm=TRUE)
  sd_rast <- rasterize(taxa_df, y = empty_ab, field = 'sds', fun=mean, na.rm=TRUE)
  
  # The default projection of leaflet is EPSG:3857.  We are going to re-project so that
  #  we don't get any strange behaviour:
  output_mean_rast <- projectRaster(mean_rast, crs = "+init=epsg:3857")
  output_sd_rast <- projectRaster(sd_rast, crs = "+init=epsg:3857")

  name_taxon <- gsub(' ', '_', taxon)
  name_taxon <- gsub('/', '_', name_taxon)
  writeRaster(stack(list(mean = mean_rast,sd = sd_rast)), filename = paste0('Shiny-App/data/rasters/',name_taxon,'.tiff'),
              overwrite=TRUE)
  
  df.output <- data.frame(xyFromCell(mean_rast, 1:ncell(mean_rast)),
                          taxon = taxon,
                          mean = getValues(mean_rast),
                          sd   = getValues(sd_rast))
  
  write.csv(df.output, file = paste0('Shiny-App/data/csvs/',name_taxon,'.csv'),
            quote = FALSE,
            row.names = FALSE)
  
  pdf(file = paste0('Shiny-App/data/pdfs/', name_taxon,'.pdf'), 
      title = "PalEON Pre-Settlement Vegetation of the Northeastern United States - Paciorek et al (2015)",
      paper = 'letter',
      compress = FALSE)
    plot(mean_rast,
         main = paste0("Proportion of ", name_taxon," in Presettlement Vegetation"))
    plot(lakes_tr, add = TRUE, col = "lightblue", border = "blue")
    plot(admin, add = TRUE)
  dev.off()
  
  taxa_df <- data.frame(xyFromCell(output_mean_rast, 1:ncell(output_mean_rast)),
                        taxon = taxon,
                        means = getValues(output_mean_rast),
                        sds   = getValues(output_mean_rast))
 
 cat('done', taxon,'\n')
 return(taxa_df)
}

all_taxa <- do.call(rbind.data.frame, lapply(taxa, taxon_to_df))

saveRDS(object = all_taxa, file = 'Shiny-App/data/all_taxa_wm.RDS')
