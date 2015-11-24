# ui.R
library(shiny)
library(leaflet)

all_taxa <- readRDS('Data/all_taxa_ll.RDS')
maptypes <- c("MapQuestOpen.Aerial",
               "Stamen.TerrainBackground",
               "Stamen.Terrain",
               "Esri.WorldImagery",
               "Esri.OceanBasemap",
               'OpenStreetMap',
               "Stamen.TonerLines")

color_palettes <- c("Blues", "BuGn",
                    "BuPu", "GnBu",
                    "Greens", "Greys",
                    "Oranges", "OrRd",
                    "PuBu", "PuBuGn",
                    "PuRd", "Purples",
                    "RdPu", "Reds",
                    "YlGn", "YlGnBu",
                    "YlOrBr", "YlOrRd")

shinyUI(fluidPage(
  # Application title:
  titlePanel("Settlement-era Tree Composition"),
  
  # The first row has two columns, one with overall map options,
  # The other with selections
  mainPanel(fluidRow(
      column(3,
           h3("Map Controls"),
           selectInput('baseTile', "Map Tileset",
                          maptypes),
           checkboxInput("labelTile", "Add Map Labels"),
           selectInput("rampPalette", "Proportion Palette",
                       color_palettes),
           selectInput("sdPalette", "St. Dev. Palette",
                       color_palettes),
           sliderInput(inputId = "zlimit",
                       label = "Display Range limits (proportion):",
                       min = 0, max = 1, value = c(0,.5), step = 0.01),
           sliderInput(inputId = "zlimit_sd",
                       label = "Uncertainty limits (proportion):",
                       min = 0.0, max = .25, value = c(0,.25), step = 0.01),
           sliderInput(inputId = "opacity",
                       label = "Opacity (%):",
                       min = 0.0, max = 100, value = 50, step = 5),
           checkboxInput(inputId = "continuous",
                         label = strong("Continuous scale"),
                         value = TRUE)),
       column(2,
           wellPanel(h3("Panel One"),
                     p(selectInput("taxon1", "PLSS Taxon", 
                                   unique(as.character(all_taxa$taxon))),
                       checkboxInput("sd_box_1", "Uncertainty St. Dev"))),
           wellPanel(h3("Panel Two"),
                     p(selectInput("taxon2", "PLSS Taxon", 
                                   unique(as.character(all_taxa$taxon))),
                       checkboxInput("sd_box_2", "Uncertainty St. Dev"))),
           wellPanel(h3("Download"),
                     p(selectInput("dlPanel", "Panel", 
                                   c('One', 'Two')),
                       selectInput("FileType", "File Type", 
                                   c('raster', 'csv'))),
                     downloadButton('downloadData', 'Download'))),
    column(7,
           leafletOutput("MapPlot1"),
           leafletOutput("MapPlot2"))
    ))
  ))
