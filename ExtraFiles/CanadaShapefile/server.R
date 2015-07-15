library(maptools)

shinyServer(function(input,output){
  Canada<-readShapeSpatial("Canada.shp")
  #USA<-readShapeSpatial("locMap2.shp")
  
  output$MapPlot<-renderPlot({
    paste("you have selected",input$selectInput)
  
    plot(Canada,col=input$mapColor)
  })
})