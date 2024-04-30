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



get_measurements <- function(item) {
  measurements <- FoodNames |> 
    filter(food_description == item) |> 
    inner_join(ConversionFactor, by = 'food_id') |> 
    inner_join(MeasureNames, by = 'measure_id') |> 
    select(measure_description)
  
  return (measurements)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    
        # Show a plot of the generated distribution
        mainPanel(
           selectInput("dropdown", label = "What did you eat today?",
                       choices = c("", unique(FoodNames$food_description))
                      ),
           selectInput("measure_list", label = "How much of it did you eat?",
                       choices = NULL),
           actionButton("add_button", "Add to your meal"),
           actionButton("remove_button", "Remove from your meal"),
           DTOutput("my_meal"),
           textOutput("mytest")
        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  selected_foods <- reactiveVal(c()) 
  selected_measures <- reactiveVal(c()) 
  
  measurements <- NULL
  
  
  observeEvent(input$dropdown, {
    measurements <- get_measurements(input$dropdown)
    updateSelectInput(session, "measure_list", choices = c("", measurements), selected = "")
  })
  
  observeEvent(input$add_button, {
    if(input$dropdown != "" & input$measure_list != ""){
      
      current_foods <- selected_foods()
      updated_foods <- c(current_foods, input$dropdown)
      selected_foods(updated_foods) 
      
      current_measures <- selected_measures()
      updated_measures <- c(current_measures, input$measure_list)
      selected_measures(updated_measures) 
    }
  })
  
  
  
  output$my_meal <- renderDT({
    my_dataframe <- data.frame(food_item = selected_foods(), measurement = selected_measures())
    datatable(my_dataframe, options = list(paging = FALSE))
  })
  
  
  observeEvent(input$remove_button, {
    if(!is.null(input$my_meal_rows_selected)){
      current_foods <- selected_foods()
      updated_foods <- current_foods[-input$my_meal_rows_selected]
      selected_foods(updated_foods) 
      
      current_measures <- selected_measures()
      updated_measures <- current_measures[-input$my_meal_rows_selected]
      selected_measures(updated_measures) 
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
