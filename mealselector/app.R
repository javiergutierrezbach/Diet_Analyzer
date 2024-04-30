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
library(DT)


# Define UI for application that draws a histogram
ui <- fluidPage(

    
        # Show a plot of the generated distribution
        mainPanel(
           selectInput("dropdown", label = "What did you eat today?",
                       choices = c("", unique(FoodNames$food_description))
                      ),
           actionButton("add_button", "Add to your meal"),
           actionButton("remove_button", "Remove from your meal"),
           DTOutput("my_meal"),
           textOutput("mytest")
        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  selected_foods <- reactiveVal(c()) 
  
  observeEvent(input$add_button, {
    if(input$dropdown != ""){
      current_foods <- selected_foods()
      updated_foods <- c(current_foods, input$dropdown)
      selected_foods(updated_foods) 
    }
  })
  
  
  
  output$my_meal <- renderDT({
    my_dataframe <- data.frame(Value = selected_foods())
    datatable(my_dataframe, options = list(paging = FALSE))
  })
  
  
  observeEvent(input$remove_button, {
    if(!is.null(input$my_meal_rows_selected)){
      current_foods <- selected_foods()
      updated_foods <- current_foods[-input$my_meal_rows_selected]
      selected_foods(updated_foods) 
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
