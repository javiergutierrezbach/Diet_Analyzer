# button press example
# Nicholas Horton (2024-05-01)
library(shiny)

ui <- shinyUI(
  fluidPage(
    tags$b("Adding values using reactiveValues() - An example"),
    selectInput(
      inputId = "food", 
      label = "Select Food to add to meal", 
      choices = c("bread", "milk", "cigarettes")
    ),
    actionButton("add1", "add selection"),
    textOutput("values")
  )
)

server <-  function(input, output, session) {
  
  selected_foods <- reactiveValues(food = character(1)) 
  # Defining & initializing the reactiveValues object
  
  observeEvent(input$add1, {
    selected_foods$food <- c(selected_foods$food, input$food) 
    # if the add button is clicked, add the food
  })
  
  output$values <- renderText({
    return(paste0(selected_foods$food, collapse = " "))
  })
}

shinyApp(ui, server)

