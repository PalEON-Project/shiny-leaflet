Shiny
=====

This repository is the source code for interactive maps of PLS tree composition maps on the web using [Shiny](http://shiny.rstudio.com/).  You can access the live page here:

**[Shiny Presettlement Vegetation - Leaflet](gandalf.berkeley.edu:3838/paciorek/setVegComp-leaflet/)**
**[Shiny Presettlement Vegetation](gandalf.berkeley.edu:3838/paciorek/setVegComp/)**

###Development

*Development by any contributor is welcome, but we ask that you follow the [Code of Conduct](code_of_conduct.md) for this project.*

+ [Simon Goring](http://github.com/SimonGoring) University of Wisconsin
+ Chris Paciorek - University of California Berkely

### Contributors
+ Michelle Mueller - Notre Dame
+ Jody Peters - Notre Dame

###Program Function
The program, as installed, loads the mean posterior output from [Paciorek *et al*](http://arxiv.org/abs/1508.07509) (in revision) and displays both the mean estimate and standard deviation of the posteriors for 23 tree taxa in the Northwestern United States at the time of settlement.  The raw data for the posterior runs comes from [Goring *et al*](http://dx.doi.org/10.1101/026575) (*in review*) and [Cogbill](http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/publications/pdfs/Cogbill_JBiogeography_2002.pdf) (2012).

###Recent Changes
+ **v0.3** - Added the ability to implement multiple base layers.  Added download methods for both CSV and GeoTIFF raster files.
+ **v0.2** - Cleaned up folders, remove old code & data files. Added multi-panel leaflet display and chained both panels to one another.
+ **v0.1** - Plotting enabled for both means & standard deviations simultaneously, sped up the initial data loading by pre-processing data, added color scheme to plotting output

###Repo Organization
Once the repo is cloned on your desktop and the required packages are installed (`shiny`, `ggplot2`, `gridExtra`, `RColorBrewer`), the app can be run from the file `RunShinyApp.R`.

`Shiny-App` is the folder where the raw code lives.  Critically, `ui.R` and `server.R` are the files that actually run the program.  `load_ncdf.R` is a pre-processing script that loads the `ncdf` posteriors from Paciorek *et al*. and saves them as a long `data.frame` in `Data\all_taxa.RDS`.