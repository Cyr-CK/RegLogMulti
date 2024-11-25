# R/module_data.R
library(DT)
library(shiny)

module_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Data Management"),
    tabsetPanel(
      tabPanel(
        "Load Data",
        fileInput(ns("file"), "Upload Data (.csv or .xlsx)"),
        checkboxInput(ns("header"), "Display Header", value = TRUE),
        DTOutput(ns("data_table"))
      ),
      tabPanel(
        "Variable Selection",
        uiOutput(ns("target_variable")),
        uiOutput(ns("explanatory_variables")),
        h4("Preview Selected Data"),
        DTOutput(ns("preview_table"))
      ),
      tabPanel(
        "Structure",
        DTOutput(ns("data_structure"))
      ),
      tabPanel(
        "Summary",
        verbatimTextOutput(ns("data_summary"))
      )
    )
  )
}
module_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Fonction pour détecter automatiquement le séparateur d'un fichier CSV
    detect_separator <- function(file_path) {
      lines <- readLines(file_path, n = 1)
      if (grepl(";", lines)) {
        return(";")
      } else if (grepl("\t", lines)) {
        return("\t")
      } else if (grepl(",", lines)) {
        return(",")
      } else {
        return(NULL)
      }
    }
    
    # Reactive expression pour charger le jeu de données
    data <- reactive({
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      
      if (ext == "csv") {
        separator <- detect_separator(input$file$datapath)
        if (is.null(separator)) {
          showNotification("Unable to detect separator. Defaulting to comma.", type = "warning")
          separator <- ","
        }
        read.csv(input$file$datapath,
                 header = input$header,
                 sep = separator)
      } else if (ext == "xlsx") {
        readxl::read_excel(input$file$datapath, col_names = input$header)
      } else {
        showNotification("Unsupported file type. Please upload a .csv or .xlsx file.", type = "error")
        return(NULL)
      }
    })
    
    # Obtenir les noms des colonnes
    column_names <- reactive({
      req(data())
      names(data())
    })
    
    # Afficher le tableau des données
    output$data_table <- renderDT({
      req(data())
      datatable(data(), options = list(pageLength = 10, autoWidth = TRUE))
    })
    
    # Interface utilisateur pour la variable cible
    output$target_variable <- renderUI({
      req(column_names())
      selectInput(ns("target"), "Select Target Variable (Qualitative):",
                  choices = column_names(), selected = column_names()[1])
    })
    
    # Interface utilisateur pour les variables explicatives
    output$explanatory_variables <- renderUI({
      req(column_names())
      selectizeInput(ns("explanatory"), "Select Explanatory Variables:",
                     choices = column_names(), multiple = TRUE, selected = column_names()[-1])
    })
    
    # Affichage des données sélectionnées
    output$preview_table <- renderDT({
      req(data(), input$target, input$explanatory)
      selected_data <- data()[, c(input$target, input$explanatory), drop = FALSE]
      datatable(selected_data, options = list(pageLength = 10, autoWidth = TRUE))
    })
    
    # Structure des données
    output$data_structure <- renderDT({
      req(data())
      structure <- data.frame(
        Column = names(data()),
        Type = sapply(data(), class)
      )
      datatable(structure, options = list(pageLength = 10, autoWidth = TRUE))
    })
    
    # Résumé statistique
    output$data_summary <- renderPrint({
      req(data())
      summary(data())
    })
  })
}

