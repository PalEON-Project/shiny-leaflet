# ui.R
all_taxa <- readRDS('data/all_taxa.RDS')

shinyUI(pageWithSidebar(

  # Application title:
  headerPanel("shapefileTest"),
  
  # Sidebar with dropdown choice of taxa:
  sidebarPanel(
    selectInput("taxon", "PLSS Taxon", unique(as.character(all_taxa$taxon)))),
    
  mainPanel(plotOutput("MapPlot")))
)
