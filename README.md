Shiny
=====

This repository is the source code for interactive maps of PLS tree composition maps on the web using [Shiny](http://shiny.rstudio.com/).

###Development
+  Michelle Mueller - Notre Dame
+ Jody Peters - Notre Dame
+ [Simon Goring](http://github.com/SimonGoring) University of Wisconsin

###Contributors
+ Chris Paciorek - University of California Berkely

###Program Function
The program, as installed, loads the mean posterior output from Paciorek *et al*. (*in prep*) and displays both the mean estimate and standard deviation of the posteriors for 23 tree taxa in the Northwestern United States at the time of settlement.  The raw data for the posterior runs comes from Goring *et al*. (*in review*) and Charlie Cogbill (REF).

###Recent Changes
+ **v0.1** - Plotting enabled for both means & standard deviations simultaneously, sped up the initial data loading by pre-processing data, added color scheme to plotting output

###TODO
+ **v0.2** - Clean up the folders, remove old code & data files.  Add shapefiles on top of the mapped output. Create file output for mapped layers (to PDF or PNG) as well as data output (as CSV?).

###Repo Organization
Once the repo is cloned on your desktop and the required packages are installed (`shiny`, `ggplot2`, `gridExtra`, `RColorBrewer`), the app can be run from the file `RunShinyApp.R`.

`Shiny-App` is the folder where the raw code lives.  Critically, `ui.R` and `server.R` are the files that actually run the program.  `load_ncdf.R` is a pre-processing script that loads the `ncdf` posteriors from Paciorek *et al*. and saves them as a long `data.frame` in `Data\all_taxa.RDS`.

Shapefiles for Canada and the US are in `Data` as well.  There are older files in `ExtraFiles` and elsewhere that need to be cleaned up.  Shapefiles should all be converted to native R format files for faster loading.