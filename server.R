#setup server for shiny
server <- function(input, output) {
  
  output$prediction <- renderText({
    ngrams(input$inputString)
  })
  
}
