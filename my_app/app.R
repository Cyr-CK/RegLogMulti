# app.R
library(shiny)
library(shinydashboard)

# Sourcing modules
source("R/module_data.R")
source("R/module_model.R")
source("R/module_training.R")

ui <- dashboardPage(
  dashboardHeader(title = "App Menu"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Model", tabName = "model", icon = icon("cogs")),
      menuItem("Training", tabName = "training", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data", module_data_ui("data_ui")),
      tabItem(tabName = "model", module_model_ui("model_ui")),
      tabItem(tabName = "training", module_training_ui("training_ui"))
    )
  )
)

server <- function(input, output, session) {
  module_data_server("data_ui")
  module_model_server("model_ui")
  module_training_server("training_ui")
}

shinyApp(ui, server)
