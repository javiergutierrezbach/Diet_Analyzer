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


# Making the data frames to include the proportions of the three main nutrients for each baseline diet
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

#---------------------------------------------------------



# Calculates the squared errors between proportions in someone's meal and a 
# baseline diet, adds them together
sum_squared_errors <- function(props, baseline) {
  props |> inner_join(baseline, by = 'nutrient_name') |> 
    mutate(sq_error = (proportions.x - proportions.y)^2) |> 
    summarise(sum_squared_errors = sum(sq_error))
}

#--------------------------------------------------------


# Uses the method above to calculate the sum of squared errors for each baseline
# and then find the baseline with the least of these values to return that diet
find_diet_fit <- function(props, athlete, normal,
                          healthnut, dessertlover,
                          carnivore, fastfoodlover) {
  
  best_fit <- 'Athlete'
  athlete_err <- sum_squared_errors(props, athlete)
  
  min <- athlete_err
  
  normal_err <- sum_squared_errors(props, normal)
  if (normal_err < min) {
    min <- normal_err
    best_fit <- 'Regular'
  }
  
  healthnut_err <- sum_squared_errors(props, healthnut)
  if (healthnut_err < min) {
    min <- healthnut_err
    best_fit <- 'Health Nut'
  }
  
  dessertlover_err <- sum_squared_errors(props, dessertlover)
  if (dessertlover_err < min) {
    min <- dessertlover_err
    best_fit <- 'Dessert Lover'
  }
  
  carnivore_err <- sum_squared_errors(props, carnivore)
  if (carnivore_err < min) {
    min <- carnivore_err
    best_fit <- 'Carnivore'
  }
  
  fastfoodlover_err <- sum_squared_errors(props, fastfoodlover)
  if (fastfoodlover_err < min) {
    min <- fastfoodlover_err
    best_fit <- 'Fast Food Lover'
  }
  
  return (best_fit)
  
} # find_diet_fit()

#----------------------------------------------------------------


# For a particular food item, it returns all the possible measurements in the 
# dataset

get_measurements <- function(item) {
  measurements <- FoodNames |> 
    filter(food_description == item) |> 
    inner_join(ConversionFactor, by = 'food_id') |> 
    inner_join(MeasureNames, by = 'measure_id') |> 
    select(measure_description)
  return (measurements)
}

#----------------------------------------------------------------

# Creates a table with the nutrient quantities in grams based on how much of  
# each food you ateand proportions as a part of the meal for Protein, Fat, and 
# Carbohydrates

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

#----------------------------------------------------------------


# Similarly, it computes the amounts of each nutrient recorded based on how much
# you ate in your meal, as well as showing the units for their measurement

quantify_nutrients <- function(meal) {
  myprops <- meal |> 
    inner_join(FoodNames, by = c('food_item' = 'food_description')) |> 
    inner_join(NutrientAmounts, by = 'food_id') |> 
    inner_join(MeasureNames, by = c('measurement' = 'measure_description')) |> 
    inner_join(ConversionFactor, by = c('food_id', 'measure_id')) |> 
    mutate(total_nutrient = nutrient_value * conversion_factor_value) |>
    inner_join(NutrientNames, by = 'nutrient_id') |> 
    select(nutrient_name, total_nutrient, nutrient_unit) |> 
    group_by(nutrient_name, nutrient_unit) |> 
    summarise(total_nutrient = sum(total_nutrient)) |> 
    arrange(desc(total_nutrient)) |> 
    select(nutrient_name, total_nutrient, nutrient_unit)
  return (myprops)
}

#----------------------------------------------------------------
#----------------------------------------------------------------



#------------ Beginning of the UI Code -------------------------

# Define UI for application as a navigation bar for the meal selector app and 
# the explore the data set app.
ui <- navbarPage(
  
        "Diet Analyzer",
        
        #----------------------------------------------------------------
        # Panel of the app responsible for the meal selector and analyzing
        tabPanel("Input your Meal",
                 
                 # Defined a spacer class for HTML divs, with margins to create 
                 # space defined in CSS.
                 tags$head(
                   tags$style(HTML("
                    .spacer {
                        margin-bottom: 20px; 
                       }
                     "))
                 ),
                 
                 #-----------------------------------------------------------
                 
                 # Makes a Row layout with different weighted column sizes for 
                 # the food and measure choosing drop downs, the add and remove
                 # buttons and the meal table in the right column.
                 
                 fluidRow(
                   
                   column(5,
                          
                          h3("Pick your food items:"),
                          
                          # Makes the drop down from unique values in the data 
                          # frame for food names in the dataset
                          selectInput("dropdown", label = "What did you eat today?",
                                      choices = c("-", unique(FoodNames$food_description))
                          ),
                          
                          # The choices for this drop down is NULL as they are 
                          # created by the get_measures method after we choose 
                          # a food item
                          
                          selectInput("measure_list", label = "How much of it did you eat?",
                                      choices = NULL),
                          
                          actionButton("add_button", "Add to your meal"),
                          actionButton("remove_button", "Remove from your meal")
                          ),
                   
                   #-----------------------------------------------------------
                   
                   column(7,
                          
                          # Heading is rendered as a text output and not normal
                          # text so that it only shows after a food is added to 
                          # the meal
                          
                          h3(textOutput("meal_heading")),
                          DTOutput("my_meal")
                          
                          )
                   
                 ), # End of Row 
                 
                 #-------------------------------------------------------------
                 
                 div(class = "spacer"),
                 
                 # Button and first output of analyze meals, a table with 
                 # the nutrient proportions
                 
                 actionButton("analyzer", "Analyze your Meal!"),
                 
                 DTOutput("my_nutrients"),
                 
                 div(class = "spacer"),
                 
                 #-------------------------------------------------------------
                 
                 # Another fluid row structure with equally weighed columns for 
                 # the pie chart on the left and the table with all nutrient
                 # values on the right
                 
                 fluidRow(
                   
                   column(6,
                          
                          # To center the pie chart veritcally with respect to 
                          # the table on the right
                          
                          div(style = "height: 160px"),
                          
                          plotOutput("nutrientPie")
                          
                          ),
                   
                   #----------------------------------------------------------
                   
                   column(6,
                          
                          h4(textOutput("nutrient_heading")),
                          div(style = "height: 25px"),
                          DTOutput("nutrientTable")
                          
                          )
                   
                 ), # End of Row
                 
                 #----------------------------------------------------------------
                 
                 div(class = "spacer"),
                 
                 h4(textOutput("yourFit")),
                 
                 div(style = "height: 30px"),
  
                 #----------------------------------------------------------------
                 
                 # This panel only displays after you analyzed your meal,
                 # so that random drop downs and titles are not displayed way in 
                 # the bottom before the content above is displayed
                 
                 conditionalPanel(
                   
                   # If analyzer button is clicked
                   condition = "input.analyzer > 0",
                   
                   h3("Compare your Meal to Baseline Diets"),
                   
                   div(style = "height: 20px"),
                   
                   selectInput("baselines", label = "Choose a Baseline Diet: ",
                               choices = c("-", "Athlete", "Regular", 
                                           "Health Nut", "Fast Food Lover", 
                                           "Carnivore", "Dessert Lover")
                   ),
                   actionButton("compare", "Compare your Meal!"),
                   plotOutput("bar")
                   
                 ), # End of Conditional Panel
                 
                 #-----------------------------------------------------------
                 
                 ), # End of Tab Panel for Meal Selector
        
        #----------------------------------------------------------------
        #----------------------------------------------------------------
        
        
        # Panel for the Explore the Data Set Application
        
        tabPanel("Explore the Data Set",
                 
                 # Row Structure with a weight of one for a left padding and
                 # a weight of 11 for the button inputs
                 fluidRow(
                   column(1),
                   column(11,
                   
                   radioButtons("DataSet", "Select a Data Set to Explore:",
                                choices = c("Food Names", "Food Groups", 
                                            "Food Sources",  "Measure Names", 
                                            "Conversion Factor", 
                                            "Nutrient Names", 
                                            "Nutrient Amounts", 
                                            "Nutrient Sources", "Yield Names", 
                                            "Yield Amounts", "Refuse Names",
                                            "Refuse Amounts"),
                                selected = "Food Names")
                   )
                 ), # End of Row Structure
                 
                 #------------------------------------------------------------
                 
                 #Displays Data Set below
                 mainPanel(
                   DTOutput("dataset")
                 )
                 ) # End of Panel
           
        
    
) # End of UI

#----------------------------------------------------------------
#----------------------------------------------------------------

#--------------- Beginning of the Server Code -------------------

# Define server logic required for interactibility of the app.
server <- function(input, output, session) {
  
  #----------------------------------------------------------------
  
  #--- Reactive Global Variables ---
  
  # Reactive data structures are used so updates are not dismissed as the server 
  # runs. They are called like functions when accessed

  # These two are the inputs by the User from the drop downs
  
  selected_foods <- reactiveVal(c()) 
  selected_measures <- reactiveVal(c()) 
  
  #----------------------------------------------------------------
  
  
  # A data frame of the meal with each food item and respective measurement
  meal <- reactive({
    data <-  data.frame(food_item = selected_foods(), measurement = selected_measures())
    return (data)
  })
  
  #----------------------------------------------------------------
  
  # A data frame of the nutrient proportions for protein, fat, and carbohydrates
  meal_nutrients <- reactive({
    
    # Calls analyse_meal method on the reactive meal global stored above
    foods <- meal()
    analyzed <- analyse_meal(foods)
    return (analyzed)
  })
  
  #----------------------------------------------------------------
  
  # Chooses the current dataframe chosen by the user to display in the Explore
  # Data Set App
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
  
  #----------------------------------------------------------------
  
  # Renders the chosen data set as output
  
  output$dataset <- renderDT({
    datatable(data = selectedData(), options = list(pageLength = 5))
  })
  
  #----------------------------------------------------------------
  
  measurements <- NULL
  
  # Updates the choices in the drop down to choose a measurement for the food, 
  # based on what food is currently selected
  observeEvent(input$dropdown, {
    measurements <- get_measurements(input$dropdown)
    updateSelectInput(session, "measure_list", choices = c("-", measurements), selected = "-")
  })
  
  
  #----------------------------------------------------------------
  
  # Add to Meal Button
  observeEvent(input$add_button, {
    
    # Renders the heading when the button is pressed
    output$meal_heading <- renderText(
      "Selected Meal: "
    )
    
    # If a food and a measure are chosen and the button is pressed,
    # then the foods and measures globals will be updated to contain them
    if(input$dropdown != "-" & input$measure_list != "-"){
      
      current_foods <- selected_foods()
      updated_foods <- c(current_foods, input$dropdown)
      selected_foods(updated_foods) 
      
      current_measures <- selected_measures()
      updated_measures <- c(current_measures, input$measure_list)
      selected_measures(updated_measures) 
    }
  }) # End of Add Button
  
  #----------------------------------------------------------------
  
  # Renders the meal table as output
  output$my_meal <- renderDT({
    datatable(meal(), options = list(paging = FALSE))
  })
  
  #----------------------------------------------------------------
  
  
  # Remove button
  observeEvent(input$remove_button, {
    
    # Similarly updates the foods and measures globals by removing the item at 
    # the index of the selected rows
    if(!is.null(input$my_meal_rows_selected)){
      current_foods <- selected_foods()
      updated_foods <- current_foods[-input$my_meal_rows_selected]
      selected_foods(updated_foods) 
      
      current_measures <- selected_measures()
      updated_measures <- current_measures[-input$my_meal_rows_selected]
      selected_measures(updated_measures) 
    }
  }) # End of Remove Button
  
  #----------------------------------------------------------------
  
  # Analyzer Button
  observeEvent(input$analyzer, {
    
    # Only if the meal is not empty, gather the nutrients from the global and 
    # round the values for neat presentation
    if (length(selected_foods) != 0) {
      myNutrients <- meal_nutrients() |> 
        mutate (across(
          where(is.numeric), round, 2
        )
        )
      
      # Render the datatable as output
      output$my_nutrients <- renderDT({
        datatable(myNutrients, options = list(paging = FALSE))
      })
      
      #----------------------------------------------------------------
      
      # Render a pie chart of the same proportions 
      output$nutrientPie <- renderPlot({
        pie(myNutrients$proportions, 
            labels = c("Carbohydrates", "Fats", "Protein"), 
            col = c("wheat3", "orange", "limegreen"), 
            main = "Your Nutrient Proportions")
      })
      
      #----------------------------------------------------------------
      
      output$nutrient_heading <- renderText(
        "Highest Amounts of Nutrients in your Meal: "
      )
      
      #----------------------------------------------------------------
      
      # Gather the current meal selected from the global and find the nutrient
      # values with quantify nutrients method, again round values
      currentMeal <- meal()
      nutrientlist <- quantify_nutrients(currentMeal) |> 
        mutate (across(
          where(is.numeric), round, 2
          )
        )
      
      # Render the table with these nutrient values
      output$nutrientTable <- renderDT({
        datatable(nutrientlist, options = list(paging = TRUE))
      })
      
      #----------------------------------------------------------------
        
      # Renders some text to say your find diet fit, by running the least 
      # sum of squared errors algorithm
      output$yourFit <- renderText(
      paste("Your meal is most comparable to the following diet: ",  
            find_diet_fit(myNutrients, athlete, normal, healthnut, dessertlover, 
                          carnivore, fastfoodlover))
      )
      
      #----------------------------------------------------------------
    
      
    }
    
  }) # End of Analyzer Button
  
  #----------------------------------------------------------------
  
  # Compare Button (Relating to Baseline Diets)
  observeEvent(input$compare, {
    
    if(input$baselines != "-"){
      
      my_frame <- NULL
      
      # Sets the frame to be used to the users input from the drop down menu.
      my_frame <- switch(
        input$baselines,
        "Athlete" = athlete,
        "Regular" = normal,
        "Health Nut" = healthnut,
        "Fast Food Lover" = fastfoodlover,
        "Carnivore" = carnivore,
        "Dessert Lover" = dessertlover
      )
      
      #----------------------------------------------------------------
      
      # Preparing the frame in order to make the bar plots
      
      # Since the meal frame and the baseline frame will be joined, we want to 
      # distinguish the two proportion values, so we rename them to the frame
      # name they come from. While this is not semantically right as they are 
      # actually proportions, once we pivot longer this will be fixed
      
      my_frame <- my_frame |> 
        rename('Baseline Diet' = proportions)
      
      # Get current nutrients frame
      myNutrients <- meal_nutrients()
      my_meal <- myNutrients |> 
        rename('Your Meal' = proportions) |> 
        select(-value)
      
      # Joins the data frames, turns the frame name as another variable instead
      # by pivoting, while getting the proportions from those columns and naming
      # them appropriately
      bardata <- my_frame |> 
        inner_join(my_meal, by = "nutrient_name") |> 
        pivot_longer(cols = c(`Your Meal`, `Baseline Diet`),
                     names_to = "frame",
                     values_to = "proportions")
      
      #----------------------------------------------------------------

      # Renders the bar chart comparing the proportions in both diets using
      # GGplot. There is a fill by frame and a position dodge argument to have
      # the respective bars of both frames side by side
      
      output$bar <- renderPlot({
        ggplot(bardata, aes(x = nutrient_name, y = proportions, fill=frame)) +
          geom_bar(stat = "identity", position=position_dodge()
                   ) +
          scale_fill_manual(values = c("springgreen3", "royalblue2")) +
          labs(
            x = 'Nutrient Name',
            y = 'Proportions',
            fill = '',
            title = "Side by Side Nutrient Proportions with the Diet of Choice"
          ) +
          theme(
            # Center the title and set font size
            plot.title = element_text(hjust = 0.5),
            text = element_text(size = 12)  # Set font size to 12
          )
      }) # End of Plot
    }
    
  }) # End of Compare Button
  
  
} # End of Server

#----------------------------------------------------------------
#----------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)
