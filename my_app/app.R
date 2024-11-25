# app.R

# Charger les bibliothèques nécessaires
library(shiny)
library(shinydashboard)
library(DT)
library(readxl)

# Charger les modules
source("R/module_data.R")
source("R/module_model.R")
source("R/module_training.R")
source("R/module_test.R")

# Interface Utilisateur
ui <- dashboardPage(
  dashboardHeader(title = "Shiny App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Model parameter", tabName = "model", icon = icon("cogs")),
      menuItem("Training", tabName = "training", icon = icon("chart-line")),
      menuItem("Test", tabName = "test", icon = icon("flask")) # Ajout de Test
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data", module_data_ui("data_ui")), # Module Data
      tabItem(tabName = "model", module_model_ui("model_ui")), # Module Model
      tabItem(tabName = "training", module_training_ui("training_ui")), # Module Training
      tabItem(tabName = "test", module_test_ui("test_ui")) # Module Test
    )
  )
)

# Serveur
server <- function(input, output, session) {
  module_data_server("data_ui")      # Serveur du module Data
  module_model_server("model_ui")   # Serveur du module Model
  module_training_server("training_ui") # Serveur du module Training
  module_test_server("test_ui")     # Serveur du module Test
}

# Lancer l'application
shinyApp(ui, server)
