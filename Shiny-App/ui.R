# ui.R
library(shiny)
library(leaflet)

all_taxa <- readRDS('Data/all_taxa_wm.RDS')

maptypes <- c("MapQuestOpen.Aerial",
               "Stamen.TerrainBackground",
               "Esri.WorldImagery",
               'OpenStreetMap')

color_palettes <- c("Blues", "GnBu",
                    "Greys", "OrRd",
                    "PuBuGn", "Reds",
                    "YlGnBu")

shinyUI(fluidPage(
  
  includeCSS("www/bootstrap.css"),
  
  # Application title:
  titlePanel("Settlement-era Tree Composition"),
  
  # The first row has two columns, one with overall map options,
  # The other with selections
  mainPanel(fluidRow(
      column(3,
           h4("Map Controls"),
           selectInput('baseTile', "Map Tileset",
                          maptypes),
           checkboxInput("labelTile", "Add Map Labels"),
           selectInput("rampPalette", "Proportion Palette",
                       color_palettes),
           selectInput("sdPalette", "St. Dev. Palette",
                       color_palettes),
           tags$div(title="Display limits for the proportional pre-settlement composition data.",
                    sliderInput(inputId = "zlimit",
                                label = "Prop. Display Limits:",
                                min = 0, max = 1, value = c(0,.5), step = 0.01)
           ),
           tags$div(title="Display limits for the standard deviation of the posterior from which the mean is calculated.",
                    sliderInput(inputId = "zlimit_sd",
                                label = "Uncertainty Limits:",
                                min = 0.0, max = .25, value = c(0,.25), step = 0.01)
           ),
           sliderInput(inputId = "opacity",
                       label = "Opacity (%):",
                       min = 0.0, max = 100, value = 50, step = 5),
           checkboxInput(inputId = "continuous",
                         label = strong("Continuous scale"),
                         value = TRUE)),
       column(2,
           wellPanel(h4("Panel Display"),
                     p(selectInput("taxon1", "Taxon One", 
                                   unique(as.character(all_taxa$taxon))),
                       checkboxInput("sd_box_1", "Uncertainty St. Dev")),
                     p(selectInput("taxon2", "Taxon Two", 
                                   unique(as.character(all_taxa$taxon))),
                       checkboxInput("sd_box_2", "Uncertainty St. Dev"))),
           wellPanel(h4("Download"),
                     p(selectInput("dlPanel", "Panel", 
                                   c('One', 'Two')),
                       selectInput("FileType", "File Type", 
                                   c('raster', 'csv', 'pdf'))),
                     downloadButton('downloadData', 'Download'))),
    column(7,
           leafletOutput("MapPlot1"),
           leafletOutput("MapPlot2"))
    ))
  ))
