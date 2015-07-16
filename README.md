Shiny
=====
These files are to create interactive maps of PLS tree composition maps on the web.  Shiny, a web application framework for R will be used to publish the maps.
Michelle Mueller, a Notre Dame undergrad created most of these files with some input from Jody Peters.  Michelle has graduated and now Jody has take over work on this.


Main Directory:
1. The RunShinyApp.R file in the main directory lets you run the Shiny App.
2. Shiny-App - this folder holds the r code for the app and some of the smaller data files used (the netcdf files are obviously not included). This is the main folder you will need.
3. ExtraFiles - this folder has extra files in it from when ND's undergrad was working on developing the code.

Shiny-App Folder:
Two folders: Data and R Code
R Code Folder:
1. maps.R is code modified from Chris Paciorek to create the composition maps using the statistical product.
2. plot.R and set_domain.R are code referred to in the maps.R.
3. ui.R had code that controls what the web page looks like (fonts, sidebar, etc)
4. server.R has the code to create the maps that will go on the webpage.  We still do have this code working properly for our composition maps.

Data Folder:
1. Has shapefile files for Canada, us albers shapefile which is the PalEON domain in the NAD83/Great Lakes and St Lawrence Albers projection, and the paleonDomain and water tifs.  The US, domain and water files are used to create the composition maps usign Chris Paciorek's statistical product.  The Canada shapefile is from code Alex Dye sent as we were trying to get shapefiles to work with Shiny (our us albers shapefile was not working in Shiny).


