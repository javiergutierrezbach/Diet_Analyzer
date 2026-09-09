## STAT231 project repo (Diet Analyser by Javier Gutierrez Bach and George Chaidemenos)

This is the GitHub repo for the end of semester project:

- report: folder for revised proposal and final report 
- data-raw: location of data to be ingested
- data: location of data to be included in the project

Your group project GitHub repo will be evaluated based on the following criteria:

- A README.md detailing all the steps to reproduce your results. I will use the 
readme to attempt to reproduce your results.
- No references to local file structure. All references should be from the main 
directory of the repo using relative pathnames.
- Appropriate naming of files and objects.
- Consistent code style and appropriate formatting.
- Helpful comments in code.
- No scrap code or files left in the repo. No passwords or personal information.


## Project Description


The project uses the Canadian Nutrient data package in R. It was built using the 
Canadian Nutrient File Compilation of Canadian Composition Data database published
by the Nutrition Research Division, Food Directoriate, Health Products and Food 
Branch at Health Canada.

You can install the package by running:

devtools::install_github("javiergutierrezbach/Canadian-Nutrient-Dataset")

Once the data package is installed in your local machine, you can find the 
interactive shiny app in the Diet Analyzer folder. To run the app, just open the 
file and click run on the R studio interface.

User manual for the app:

There are two interfaces for this app that you can choose between in the navigation
bar above. The 'Explore your Data Set' tab is a way for you to look throughout 
the tables in the data set by just choosing from the buttons above.

For the main interface 'Input your Meal', there are plenty of buttons and input 
bars to discuss. If you have a particular meal in mind that you want to 
explore in terms of its nutritional value, then here is the place to do it. 

You will have two drop down menus. The first labeled 'What did you eat today?' where you 
can choose a food item from the ones available in the database. You can clear the
text and write in your own foods and see if it is in the database, which it most 
probably will. Once you select a food item, the 'How much did you eat?' will reveal 
options of the possible measurements of the food you can choose. Pick what most 
closely aligns to how much you ate! Once you have picked a measurement, click the
'Add to your meal' button so that it gets added to it. Your current meal will be 
displayed on a table on the right. Keep choosing food items in this way until your 
meal is complete. If you make a mistake, you can select a food item on the meal 
table and click the 'Remove from Meal' button.

Once your meal is complete, you will want to see its nutritional values. To do this,
simply press the 'Analyze your Meal' button. This will display three things as well 
as further options. Right below it you will see a table with the quantities in grams 
of your meal for proteins, fats, and carbohydrates, as well as their general proportion 
in the composition of the meal. This same data is displayed in a pie chart below 
this to the left. On the right, you will see another table with all of the nutrients 
available in the data set and their corresponding values and units. You can search 
for the nutrient you care about or scroll through. Additionally, we run our least 
square errors algorithm to find which of our baseline diets' proportions ressembles 
the most to those in your meal.

After looking at the nutrients, you can actually compare your meal to the baseline 
diets. Each of which has standardized values for the protein, carbohydrates and fats 
values (athletes will have more protein and carbohydrate intake, for example). In 
the final dropdown menu, you can choose a specific baseline diets and compare your 
proportions to those of the diet in a side by side bar chart. The proportions of 
your meal are presented in blue, while those of the diet are in green. It provides 
a good indicator of what you need to eat less or more depending on your goals.

That is all there is to know to use our interactive Diet Analyzer App! Go investigate 
your favorite meals and become a better version of yourself!

