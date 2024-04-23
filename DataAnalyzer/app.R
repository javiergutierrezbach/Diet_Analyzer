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

    # Application title
    titlePanel("Canadian Diet Analyser"),
    fluidRow(
      
      radioButtons("DataSet", "Select a Data Set to Explore:",
                   choices = c("Food Names", "Food Groups", "Food Sources", 
                               "Measure Names", "Conversion Factor", 
                               "Nutrient Names", "Nutrient Amounts", 
                               "Nutrient Sources", "Yield Names", 
                               "Yield Amounts", "Refuse Names", "Refuse Amounts"),
                   selected = "Food Names")
    ),
    mainPanel(
      DTOutput("myTable")
    )
    # Sidebar with a slider input for number of bins 
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    
    selectedData <- reactive({
      switch(input$DataSet,
           "Food Names" = FoodNames,
           "Food Groups" = FoodGroup,
           "Food Sources" = FoodSources,
           "Measure Names" = MeasureNames,
           "Conversion Factor" = ConversionFactor,
           "Nutrient Names" = NutrientNames,
           "Nutrient Amounts" = NutrientAmounts,
           "Nutrient Sources" = NutrientSources,
           "Yield Names" = YieldNames,
           "Yield Amounts" = YieldAmounts,
           "Refuse Names" = RefuseNames,
           "Refuse Amounts" = RefuseAmounts)
   })
  
    output$myTable <- renderDT({
      datatable(data = selectedData(), options = list(pageLength = 5))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
