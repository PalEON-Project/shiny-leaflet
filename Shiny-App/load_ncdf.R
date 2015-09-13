library(raster)
library(ncdf4)

domain <- nc_open('Data/composition_v0.3.nc', write = FALSE)

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
  
  mean_ll <- projectRaster(mean_rast, empty_ll)
  sd_ll   <- projectRaster(sd_rast,   empty_ll)
  
  taxa_df <- data.frame(xyFromCell(mean_ll, 1:ncell(mean_ll)),
                        taxon = taxon,
                        means = getValues(mean_ll),
                        sds   = getValues(sd_ll))
  taxa_df
  
}

all_taxa <- do.call(rbind.data.frame,lapply(taxa, taxon_to_df))

saveRDS(object = all_taxa, file = 'Data/all_taxa_ll.RDS')
