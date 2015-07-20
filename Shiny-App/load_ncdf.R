library(raster)
library(ncdf4)

domain <- nc_open('data/composition_v0.3.nc', write = FALSE)

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
  
  taxa_df
  
}

all_taxa <- do.call(rbind.data.frame,lapply(taxa, taxon_to_df))

saveRDS(object = all_taxa, file = 'data/all_taxa.RDS')
