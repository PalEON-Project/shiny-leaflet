# ui.R


shinyUI(pageWithSidebar(
headerPanel("shapefileTest"),
sidebarPanel(
  selectInput("mapColor","choose color of Canada:",choices=c("gray","blue","yellow"))),
mainPanel(plotOutput("MapPlot",height="800px"))))



#(shinyUI(fluidPage(
 # titlePanel("Composition Map"),
  
  #sidebarLayout(
   # pageWithSidebar(
    # headerPanel("shapefileTest"),
  #  sidebarPanel(
   #     selectInput("mapColor","choose color of US:",choices=c("gray","blue","yellow")),
    #    h2("Taxa"),
     #   h6("Create demographic maps with 
      #  information regarding Taxa Composition."),
      #  br(),
       # br(),
       # selectInput("var", 
        #            label = "Choose a variable to display",
         #           choices = c("Ash", "Atlantic white cedar",
         #                       "Beech", "Birch"),
          #          selected = "Ash"),
        #br(),
        
        #sliderInput("n", 
                  #  "Range:", 
                    #value = 500,
                    #min = 1, 
                    #max = 1000)))),
    #))
    #  mainPanel(plotOutput("MapPlot",height="800px")),
      
    #))))