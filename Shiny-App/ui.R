# ui.R
library(shiny)
#library(shinydashboard)
library(leaflet)

all_taxa <- readRDS('data/all_taxa_wm.RDS')

maptypes <- c("MapQuestOpen.Aerial",
               "Stamen.TerrainBackground",
               "Esri.WorldImagery",
               'OpenStreetMap')

color_palettes <- c("Blues", "GnBu",
                    "Greys", "OrRd",
                    "PuBuGn", "Reds",
                    "YlGnBu")

#header <- dashboardHeader(title = "PalEON Vegetation")

shinyUI(fluidPage(
  
  includeCSS("www/bootstrap.css"),
  # Application title:
  titlePanel(title = "Settlement-era Tree Composition",
             windowTitle = "Settlement-era Tree Composition"),
  
  # The first row has two columns, one with overall map options,
  # The other with selections
  mainPanel(fluidRow(
      column(3,
           h4("Map Controls"),
           tags$div(title="Choose the background layer to display.",
                    selectInput('baseTile', "Map Tileset",
                          maptypes)),
           tags$div(title="Do you want to add city/country labels to the map?",
                    checkboxInput("labelTile", "Add Map Labels")),
           tags$div(title="Color palette for the means of the posterior draws for the display taxa.",
                    selectInput("rampPalette", "Proportion Palette",
                       color_palettes)),
           tags$div(title="Color palette for displaying the standard deviation of the posterior draws.",
                    selectInput("sdPalette", "St. Dev. Palette",
                       color_palettes)),
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
           tags$div(title="Choose the level of opacity for the display data.",
                    sliderInput(inputId = "opacity",
                       label = "Opacity (%):",
                       min = 0.0, max = 100, value = 50, step = 5)),
           tags$div(title="Would you prefer a continuous (default) or discrete scale (7 levels) for mapping.",
                    checkboxInput(inputId = "continuous",
                         label = strong("Continuous scale"),
                         value = TRUE))),
       column(2,
           wellPanel(h4("Panel Display"),
                     p(tags$div(title="Select the taxon for display for the upper panel.",
                                selectInput("taxon1", "Taxon One", 
                                   unique(as.character(all_taxa$taxon)))),
                       tags$div(title="Would you like to see the posterior mean (unchecked) or the standard deviation of the posterior draws?",
                                checkboxInput("sd_box_1", "St. Dev"))),
                     p(tags$div(title="Select the taxon for display for the lower panel.",
                                selectInput("taxon2", "Taxon Two", 
                                   unique(as.character(all_taxa$taxon))),
                                tags$div(title="Would you like to see the posterior mean (unchecked) or the standard deviation of the posterior draws?",
                                         checkboxInput("sd_box_2", "St. Dev"))))),
           wellPanel(h4("Download"),
                     p(tags$div(title="Choose to download the data from the upper or lower panel (both the mean & SD will be downloaded)",
                                selectInput("dlPanel", "Panel", 
                                   c('One', 'Two'))),
                       tags$div(title="Select the file type for download (compressed in a zipped file).",
                                selectInput("FileType", "File Type", 
                                            c('raster', 'csv', 'pdf'))),
                       tags$div(title="Click this button to download the data you want.",
                                downloadButton('downloadData', 'Download'))))),
    column(7,
           leafletOutput("MapPlot1"),
           leafletOutput("MapPlot2"))
    ))
  ))
