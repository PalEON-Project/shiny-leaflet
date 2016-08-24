# ui.R
library(shiny)
library(shinydashboard)
library(leaflet)

all_taxa <- readRDS('data/all_taxa_wm.RDS')

maptypes <- c("Esri.WorldImagery",
              "Stamen.TerrainBackground",
              "OpenStreetMap")

color_palettes <- c("Blues", "GnBu",
                    "Greys", "OrRd",
                    "PuBuGn", "Reds",
                    "YlGnBu")

body <- dashboardBody(
  div(class = "outer",
  tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 250px; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
  leafletOutput("MapPlot1", height = '50%'),
  leafletOutput("MapPlot2", height = '50%'))
)

header <- dashboardHeader(title = "PalEON Vegetation",
                          titleWidth = 250)

sidebar <- dashboardSidebar(
  width = 250,
  tags$head(
    tags$style(HTML(".sidebar { height: 95vh; width: 250px; overflow-y: auto;}"))
    ),
  
  HTML("<br>"),
  
  box(title = 'Species Display',
      tags$div(title = "Display taxon for the upper panel",
               selectInput("taxon1", "Taxon One", 
                  unique(as.character(all_taxa$taxon)))),
      tags$div(title = "Display the standard deviation instead of the posterior mean.",
               checkboxInput("sd_box_1", "Standard Deviation")),
      tags$div(title = "Display taxon for the lower panel",
               selectInput("taxon2", "Taxon Two", 
                  unique(as.character(all_taxa$taxon)))),
      tags$div(title = "Display the standard deviation instead of the posterior mean.",
               checkboxInput("sd_box_2", "Standard Deviation")),
      collapsible = TRUE, 
      collapsed = FALSE,
      width = NULL,
      color = 'blue',
      solidHeader = TRUE,
      background = 'light-blue'),
  
  box(title = "Map Backgrounds",
      icon  = 'map-o',
      tags$div(title = "Select the leaflet basemap to display.",
               selectInput('baseTile', "Map Tileset",
                         maptypes)),
      tags$div(title = "Do you want to overlay placename variables?",
               checkboxInput("labelTile", "Add Map Labels")), 
      collapsible = TRUE, 
      collapsed = TRUE,
      width = NULL,
      solidHeader = TRUE,
      color = 'blue',
      background = 'light-blue'),
    
  box(title = "Plot Options",
      tags$head(
        tags$style(HTML(".irs-bar { height: 8px; top: 25px; border-top: 1px solid #428bca; 
                                    border-bottom: 1px solid #428bca; background: #ffffff; } 
                        .irs-bar-edge { height: 8px; top: 25px; width: 14px; 
                                        border: 1px solid #428bca; border-right: 0; 
                                        background: #ffffff; border-radius: 16px 0 0 16px; 
                                        -moz-border-radius: 16px 0 0 16px; }
                        .irs-grid-text { position: absolute; bottom: 0; left: 0; 
                                         white-space: nowrap; text-align: center; font-size: 9px; 
                                         line-height: 9px; padding: 0 3px; color: #000; }"))),
      tags$div(title = "Adjust the display limits for the mean posterior proportion data.",
               sliderInput(inputId = "zlimit",
                  label = "Prop. Display Limits:",
                  min = 0, max = 1, value = c(0,.5), step = 0.01)),
      selectInput("rampPalette", "Proportion Palette",
                       color_palettes),
      tags$div(title = "Adjust the display limits for the posterior standard deviation data.",
               sliderInput(inputId = "zlimit_sd",
                  label = "Standard Deviation Display Limits:",
                  min = 0, max = 1, value = c(0,.5), step = 0.01)),
      selectInput("sdPalette", "Standard Deviation Display Limits:",
                           color_palettes),
      tags$div(title = "Adjust the overlay data opacity (from 0 - 100, where 0 is totally transparent).",
               sliderInput(inputId = "opacity",
                           label = "Opacity (%):",
                           min = 0.0, max = 100, value = 50, step = 5)),
      tags$div(title = "Display the data as a continuous or discrete scale.",
               checkboxInput(inputId = "continuous",
                             label = strong("Continuous scale"),
                             value = TRUE)),
      collapsible = TRUE, 
      collapsed = TRUE,
      width = NULL,
      solidHeader = TRUE,
      color = 'blue',
      background = 'light-blue'),
    
  box(title = 'Download',
      tags$div(title = "For which panel would you like to download data (\"One\" is upper)?",
               selectInput("dlPanel", "Panel", 
                       c('One', 'Two'))),
      tags$div(title = "Select the download filetype.  PDF is image only.",
               selectInput("FileType", "File Type", 
                           c('raster', 'csv', 'pdf'))),
      tags$style(type = 'text/css', "#btn { color:black;}"),
      tags$div(title = "Click to download the data. Button looks grey, but works :)",
               downloadButton('downloadData', 'Download', class = "color:#ffffff;")),
      collapsible = TRUE, 
      collapsed = TRUE,
      width = NULL,
      solidHeader = TRUE,
      color = 'blue',
      background = 'light-blue')
)


dashboardPage(header, sidebar, body)