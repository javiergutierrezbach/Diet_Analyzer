#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(dplyr)
library(shiny)
library(CanadianNutrient)
library(DT)
library(ggplot2)
library(tidyr)


athlete <- tibble(
  nutrient_name = c("PROTEIN", "FAT (TOTAL LIPIDS)",
                    "CARBOHYDRATE, TOTAL (BY DIFFERENCE)"),
  proportions = c(30, 20, 50.0)
)


normal <- tibble(
  nutrient_name = c("PROTEIN", "FAT (TOTAL LIPIDS)",
                    "CARBOHYDRATE, TOTAL (BY DIFFERENCE)"),
  proportions = c(25, 25, 50)
)

healthnut <- tibble(
  nutrient_name = c("PROTEIN", "FAT (TOTAL LIPIDS)", 
                    "CARBOHYDRATE, TOTAL (BY DIFFERENCE)"),
  proportions = c(35, 10, 55)
)

fastfoodlover<- tibble(
  nutrient_name = c("PROTEIN", "FAT (TOTAL LIPIDS)", 
                    "CARBOHYDRATE, TOTAL (BY DIFFERENCE)"),
  proportions = c(20, 50, 30)
)

carnivore <- tibble(
  nutrient_name = c("PROTEIN", "FAT (TOTAL LIPIDS)", 
                    "CARBOHYDRATE, TOTAL (BY DIFFERENCE)"),
  proportions = c(60, 30, 10)
)

dessertlover <- tibble(
  nutrient_name = c("PROTEIN", "FAT (TOTAL LIPIDS)", 
                    "CARBOHYDRATE, TOTAL (BY DIFFERENCE)"),
  proportions = c(7.5, 32.5, 60.0)
)

sum_squared_errors <- function(props, baseline) {
  props |> inner_join(baseline, by = 'nutrient_name') |> 
    mutate(sq_error = (proportions.x - proportions.y)^2) |> 
    summarise(sum_squared_errors = sum(sq_error))
}

find_diet_fit <- function(props, athlete, normal,
                          healthnut, dessertlover,
                          carnivore, fastfoodlover) {
  
  best_fit <- 'athlete'
  athlete_err <- sum_squared_errors(props, athlete)
  
  min <- athlete_err
  
  normal_err <- sum_squared_errors(props, normal)
  if (normal_err < min) {
    min <- normal_err
    best_fit <- 'normal'
  }
  
  healthnut_err <- sum_squared_errors(props, healthnut)
  if (healthnut_err < min) {
    min <- healthnut_err
    best_fit <- 'healthnut'
  }
  
  dessertlover_err <- sum_squared_errors(props, dessertlover)
  if (dessertlover_err < min) {
    min <- dessertlover_err
    best_fit <- 'dessertlover'
  }
  
  carnivore_err <- sum_squared_errors(props, carnivore)
  if (carnivore_err < min) {
    min <- carnivore_err
    best_fit <- 'carnivore'
  }
  
  fastfoodlover_err <- sum_squared_errors(props, fastfoodlover)
  if (fastfoodlover_err < min) {
    min <- fastfoodlover_err
    best_fit <- 'fastfoodlover'
  }
  
  return (best_fit)
  
}

get_measurements <- function(item) {
  measurements <- FoodNames |> 
    filter(food_description == item) |> 
    inner_join(ConversionFactor, by = 'food_id') |> 
    inner_join(MeasureNames, by = 'measure_id') |> 
    select(measure_description)
  return (measurements)
}

analyse_meal <- function(Meal) {
  Nutrients <- Meal |> 
    inner_join(FoodNames, by = c('food_item' = 'food_description')) |> 
    inner_join(NutrientAmounts, by = 'food_id') |> 
    inner_join(NutrientNames, by = 'nutrient_id') |> 
    filter(nutrient_name == 'PROTEIN' | nutrient_name == 'FAT (TOTAL LIPIDS)' | nutrient_name == 'CARBOHYDRATE, TOTAL (BY DIFFERENCE)') |> 
    inner_join(MeasureNames, by = c('measurement' = 'measure_description')) |> 
    inner_join(ConversionFactor, by = c('measure_id', 'food_id')) |> 
    mutate(new_value = nutrient_value * conversion_factor_value) |> 
    group_by(nutrient_name) |>
    summarise("value" = sum(new_value)) |>
    mutate(proportions = value * 100 / sum(value)) |> 
    select(nutrient_name, value, proportions)
  
  return (Nutrients)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    
        # Show a plot of the generated distribution
        mainPanel(
           selectInput("dropdown", label = "What did you eat today?",
                       choices = c("-", unique(FoodNames$food_description))
                      ),
           selectInput("measure_list", label = "How much of it did you eat?",
                       choices = NULL),
           actionButton("add_button", "Add to your meal"),
           actionButton("remove_button", "Remove from your meal"),
           DTOutput("my_meal"),
           actionButton("analyzer", "Analyze your Meal!"),
           DTOutput("my_nutrients"),
           plotOutput("nutrientPie"),
           textOutput("yourFit"),
           selectInput("baselines", label = "Choose a Baseline Diet",
                       choices = c("-", "athlete", "normal", "healthnut", "fastfoodlover", "carnivore", "dessertlover")
           ),
           actionButton("compare", "Compare your Meal!"),
           plotOutput("bar")
           
        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  selected_foods <- reactiveVal(c()) 
  selected_measures <- reactiveVal(c()) 
  
  measurements <- NULL
  
  observeEvent(input$dropdown, {
    measurements <- get_measurements(input$dropdown)
    updateSelectInput(session, "measure_list", choices = c("-", measurements), selected = "-")
  })
  
  observeEvent(input$add_button, {
    if(input$dropdown != "-" & input$measure_list != "-"){
      
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
  
  observeEvent(input$analyzer, {
    my_dataframe <- data.frame(food_item = selected_foods(), measurement = selected_measures())
    if (nrow(my_dataframe) != 0) {
      myNutrients <- analyse_meal(my_dataframe)
      output$my_nutrients <- renderDT({
        datatable(myNutrients, options = list(paging = FALSE))
      })
      
      output$nutrientPie <- renderPlot({
        pie(myNutrients$proportions, labels = myNutrients$nutrient_name, 
            col = c("wheat3", "orange", "limegreen"), main = "Your Nutrient Proportions")
      })
    output$yourFit <- renderText(
      paste("Your meal is most comparable to the following diet: ",  find_diet_fit(myNutrients, athlete, normal,
                                                                                   healthnut, dessertlover,
                                                                                   carnivore, fastfoodlover))
    )
      
    }
    
  })
  
  observeEvent(input$compare, {
    if(input$baselines != "-"){
      
      my_frame <- NULL
      
      switch(
        input$baselines,
        "athlete" = my_frame <- athlete,
        "normal" = my_frame <- normal,
        "healthnut" = my_frame <- healthnut,
        "fastfoodlover" = my_frame <- fastfoodlover,
        "carnivore" = my_frame <- carnivore,
        "dessertlover" = my_frame <- dessertlover
      )
      
      my_frame <- my_frame |> rename('baseline' = proportions)
      my_meal <- myNutrients |> rename('meal' = proportions)
      
      bardata <- my_frame |> 
        inner_join(my_meal, by = "nutrient_name") |> 
        pivot_longer(cols = c(baseline, meal),
                     names_to = "frame",
                     values_to = "proportions")
      
      output$bar <- renderPlot({
        ggplot(bardata, aes(x = nutrient_name, y = proportions, fill=frame)) +
          geom_bar(stat = "identity", position=position_dodge()
                   ) +
          scale_fill_manual(values = c("springgreen3", "royalblue2")) +
          labs(
            x = 'Nutrient Name',
            y = 'Proportions',
            fill = '',
            title = "Your Meal's Nutrient Proportions Compared to a Baseline Diet"
          )
      })
    }
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
