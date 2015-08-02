# ui.R
library(shiny)

all_taxa <- readRDS('data/all_taxa.RDS')

shinyUI(pageWithSidebar(

  # Application title:
  headerPanel("Settlement-era Tree Composition"),
  
  # Sidebar with dropdown choice of taxa:
  sidebarPanel(
    selectInput("taxon", "PLSS Taxon", unique(as.character(all_taxa$taxon))),
      sliderInput(inputId = "zlimit",
                  label = "Scale upper limit:",
                  min = 0.05, max = 1, value = .5, step = 0.05),
     checkboxInput(inputId = "continuous",
      label = strong("Continuous scale"),
      value = TRUE),
    width = 3), 
  mainPanel(plotOutput("MapPlot", width = "800px"), width = 9))
)
