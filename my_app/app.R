# app.R

# Charger les bibliothèques nécessaires
library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(R6)

# Charger les modules
source("R/module_data.R")
source("R/module_preprocessing.R")
source("R/module_training.R")
source("R/module_test.R")
source("R/preprocessing/FAMD_.R")
source("R/preprocessing/one_hot_encoder.R")
source("R/preprocessing/standard_scaler.R")
source("R/preprocessing/min_max_scaler.R")
source("R/preprocessing/robust_scaler.R")
source("R/preprocessing/simple_quanti_imputer.R")
source("R/preprocessing/simple_quali_imputer.R")


# Interface Utilisateur
ui <- dashboardPage(
  dashboardHeader(title = "Shiny App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Preprocessing", tabName = "preprocessing", icon = icon("cogs")),
      menuItem("Training", tabName = "training", icon = icon("chart-line")),
      menuItem("Test", tabName = "test", icon = icon("flask"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data", module_data_ui("data_ui")), # Module Data
      tabItem(tabName = "preprocessing", module_preprocessing_ui("preprocessing_ui")), # Module Preprocessing
      tabItem(tabName = "training", module_training_ui("training_ui")), # Module Training
      tabItem(tabName = "test", module_test_ui("test_ui")) # Module Test
    )
  )
)

# Serveur
server <- function(input, output, session) {
  data_output <- module_data_server("data_ui")  # Data module
  preprocessing_output <- module_preprocessing_server("preprocessing_ui", data = data_output$data)  # Preprocessing module
  
  module_training_server(
    "training_ui", 
    data = preprocessing_output$data,  # Preprocessed data
    target_variable = data_output$target_variable, 
    explanatory_variables = data_output$explanatory_variables
  )
  
  module_test_server("test_ui")  # Test module
}



# Lancer l'application
shinyApp(ui, server)
