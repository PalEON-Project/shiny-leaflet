library(maptools)

shinyServer(function(input,output){
  USA<-readShapeSpatial("us-alb.shp")
  #USA<-readShapeSpatial("locMap2.shp")
  
  output$MapPlot<-renderPlot({
    paste("you have selected",input$selectInput)
  
    plot(USA,col=input$mapColor)
  })
})