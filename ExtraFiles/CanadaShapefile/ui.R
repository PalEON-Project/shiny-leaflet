shinyUI(pageWithSidebar(
  headerPanel("shapefileTest"),
  sidebarPanel(
    selectInput("mapColor","choose color of Canada:",choices=c("red","blue","yellow"))),
                mainPanel(plotOutput("MapPlot",height="800px"))))
