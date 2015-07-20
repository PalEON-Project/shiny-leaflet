# ui.R

shinyUI(pageWithSidebar(

  # Application title:
  headerPanel("shapefileTest"),
  
  # Sidebar with dropdown choice of taxa:
  sidebarPanel(
    selectInput("taxon", "PLSS Taxon", unique(all_taxa$taxon))),
    
  mainPanel(plotOutput("MapPlot")))
)
