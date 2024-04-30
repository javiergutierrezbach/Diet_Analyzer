#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(CanadianNutrient)




# Define UI for application that draws a histogram
ui <- fluidPage(

    
        # Show a plot of the generated distribution
        mainPanel(
           selectInput("dropdown", label = "What did you eat today?",
                       choices = c("", unique(FoodNames$food_description))
                      ),
           actionButton("add_button", "Add to your meal"),
           textOutput("food_list")
        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  selected_foods <- reactiveValues(items = c())
    
       

  observeEvent(input$add_button, {
    if(input$dropdown != ""){
      selected_foods$items <- c(selected_foods$items, input$dropdown)
    }
  })
  
  
  output$food_list <- renderText({
    paste("List: ", paste(selected_foods$items, collapse = "\n"))
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
