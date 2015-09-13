# ui.R
library(shiny)
library(leaflet)

all_taxa <- readRDS('Data/all_taxa_ll.RDS')

shinyUI(fluidPage(
  # Application title:
  titlePanel("Settlement-era Tree Composition"),
  
  # Sidebar with dropdown choice of taxa:
  sidebarPanel(
    selectInput("taxon1", "PLSS Taxon", unique(as.character(all_taxa$taxon))),
    selectInput("taxon2", "PLSS Second Taxon", c("None", unique(as.character(all_taxa$taxon)))),
      sliderInput(inputId = "zlimit",
                  label = "Upper limit (estimates):",
                  min = 0.05, max = 1, value = .5, step = 0.1),
      sliderInput(inputId = "zlimit_sd",
                  label = "Upper limit (Uncertainty):",
                  min = 0.0, max = .25, value = .25, step = 0.05),
      sliderInput(inputId = "transparancy",
                  label = "Transparancy:",
                  min = 0.0, max = 1, value = 0.5, step = 0.05),
     checkboxInput(inputId = "continuous",
      label = strong("Continuous scale"),
      value = TRUE),
    width = 2),
    
  mainPanel(fluidRow(
    column(5,leafletOutput("MapPlot")),
    column(5,leafletOutput("MapPlot2"))
    ),
    conditionalPanel(condition="input.taxon2!=='None'",
                     fluidRow(
                      column(5,leafletOutput("MapPlot3")),
                      column(5,leafletOutput("MapPlot4"))
                    ))
  )))
