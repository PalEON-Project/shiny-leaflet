shinyUI(pageWithSidebar(
  headerPanel("shapefileTest"),
  sidebarPanel(
    selectInput("mapColor","choose color of USA:",choices=c("gray","blue","yellow"))),
                mainPanel(plotOutput("MapPlot",height="800px"))))
