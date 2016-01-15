csv_files <- list.files('Shiny-App/data/csvs/', full.names = TRUE)
csv_files_short <- list.files('Shiny-App/data/csvs/')

tif_files <- list.files('Shiny-App/data/rasters/', full.names = TRUE)
tif_files_short <- list.files('Shiny-App/data/rasters/')

description <- list.files('Shiny-App/data/', pattern = 'data_', full.names = TRUE)

library(utils)

for(i in 1:length(csv_files)){
  # Zip the CSV:
  taxon_name <- substr(csv_files_short[i], 1, regexpr('.', csv_files_short[i], fixed = TRUE)-1)
  zip(paste0('Shiny-App/data/zips/csv/',taxon_name,'_csv.zip'), files = c(csv_files[i], description))
  
  #taxon_name <- substr(tif_files_short[i], 1, regexpr('.', tif_files_short[i], fixed = TRUE)-1)
  #zip(paste0('Shiny-App/data/zips/raster/',taxon_name,'_raster.zip'), files = c(tif_files[i], description))
}
