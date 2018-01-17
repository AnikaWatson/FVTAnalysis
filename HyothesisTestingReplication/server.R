library(shiny)
shinyServer(function(input, output){
  output$out <- renderText(input$n)
})