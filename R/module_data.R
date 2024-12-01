# R/module_data.R
library(shiny)
library(magrittr)


`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}


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
        "Information",
        DTOutput(ns("data_structure"))
      ),
      tabPanel(
        "Missing Data",
        h4("Imputation of Missing Data"),
        selectInput(ns("missing_imputation_type"), "Select Imputation Type:",
                    choices = c("Simple Qualitative Imputer", "Simple Quantitative Imputer"),
                    selected = "Simple Quantitative Imputer"),
        actionButton(ns("apply_missing_imputation"), "Apply Imputation"),
        h4("Imputed Data"),
        DTOutput(ns("imputed_data"))
      ),
      tabPanel(
        "Variable Selection",
        uiOutput(ns("target_variable")),
        uiOutput(ns("explanatory_variables")),
        h4("Preview Selected Data"),
        DTOutput(ns("preview_table"))
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
    raw_data <- reactive({
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
    
    # Placeholders pour stocker les données imputées
    imputed_qualitative <- reactiveVal(NULL)
    imputed_quantitative <- reactiveVal(NULL)
    
    # Prétraitement des données pour rendre les valeurs manquantes visibles
    processed_data <- reactive({
      req(raw_data())
      df <- raw_data()
      
      # Identifier les colonnes qualitatives
      qualitative_cols <- names(df)[sapply(df, is.character) | sapply(df, is.factor)]
      
      # Remplacer les valeurs NA ou vides par "NA" dans les colonnes qualitatives
      df[qualitative_cols] <- lapply(df[qualitative_cols], function(col) {
        ifelse(is.na(col) | col == "", NA, col)
      })
      
      return(df)
    })
    
    # Mise à jour des données imputées lorsqu'une imputation est appliquée
    observeEvent(input$apply_missing_imputation, {
      req(processed_data())
      data <- processed_data()
      
      # Identifier les colonnes qualitatives et quantitatives
      qualitative_cols <- names(data)[sapply(data, is.character) | sapply(data, is.factor)]
      quantitative_cols <- names(data)[sapply(data, is.numeric)]
      
      # Appliquer l'imputation en fonction du type sélectionné
      imputation_type <- input$missing_imputation_type
      if (imputation_type == "Simple Qualitative Imputer") {
        if (length(qualitative_cols) == 0) {
          showNotification("No qualitative variables to impute.", type = "error")
          return()
        }
        imputer <- Simple.Quali.Imputer$new()  # Changez par "probability" si besoin
        tryCatch({
          imputed_qual <- imputer$fit_transform(data, qualitative_cols)
          imputed_qualitative(imputed_qual)
          showNotification("Qualitative imputation completed successfully.", type = "message")
        }, error = function(e) {
          showNotification(paste("Error during qualitative imputation:", e$message), type = "error")
        })
      } else if (imputation_type == "Simple Quantitative Imputer") {
        if (length(quantitative_cols) == 0) {
          showNotification("No quantitative variables to impute.", type = "error")
          return()
        }
        tryCatch({
          imputed_quanti <- lapply(data[quantitative_cols], function(col) {
            ifelse(is.na(col), mean(col, na.rm = TRUE), col)
          })
          imputed_quanti <- as.data.frame(imputed_quanti)
          colnames(imputed_quanti) <- quantitative_cols
          imputed_quantitative(imputed_quanti)
          showNotification("Quantitative imputation completed successfully.", type = "message")
        }, error = function(e) {
          showNotification(paste("Error during quantitative imputation:", e$message), type = "error")
        })
      }
    })
    
    # Fusionner les données imputées pour produire la version finale
    final_data <- reactive({
      data <- processed_data()
      imputed_qual <- imputed_qualitative() %||% data[sapply(data, is.character) | sapply(data, is.factor)]
      imputed_quanti <- imputed_quantitative() %||% data[sapply(data, is.numeric)]
      
      # Remplacer les colonnes imputées dans les données originales
      data[names(imputed_qual)] <- imputed_qual
      data[names(imputed_quanti)] <- imputed_quanti
      
      return(data)
    })
    
    # Mise à jour des visualisations
    output$data_table <- renderDT({
      req(final_data())
      datatable(final_data(), options = list(pageLength = 10, autoWidth = TRUE))
    })
    
    output$data_structure <- renderDT({
      req(final_data())
      df <- final_data()
      structure <- data.frame(
        Column = names(df),
        Type = sapply(df, class),
        TotalValues = sapply(df, length),
        MissingValues = sapply(df, function(x) {
          if (is.character(x) || is.factor(x)) {
            sum(x == "NA")  # Comptabiliser les "NA" explicites
          } else {
            sum(is.na(x))
          }
        })
      )
      datatable(structure, options = list(pageLength = 10, autoWidth = TRUE),
                rownames = FALSE)
    })
    
    output$data_summary <- renderPrint({
      req(final_data())
      summary(final_data())
    })
    
    output$target_variable <- renderUI({
      req(names(final_data()))
      selectInput(ns("target"), "Select Target Variable (Qualitative):",
                  choices = names(final_data()), selected = names(final_data())[1])
    })
    
    output$explanatory_variables <- renderUI({
      req(names(final_data()))
      selectizeInput(ns("explanatory"), "Select Explanatory Variables:",
                     choices = names(final_data()), multiple = TRUE, selected = names(final_data())[-1])
    })
    
    output$preview_table <- renderDT({
      req(final_data(), input$target, input$explanatory)
      selected_data <- final_data()[, c(input$target, input$explanatory), drop = FALSE]
      datatable(selected_data, options = list(pageLength = 10, autoWidth = TRUE))
    })
    
    output$imputed_data <- renderDT({
      req(final_data())
      datatable(final_data(), options = list(pageLength = 10, autoWidth = TRUE))
    })
    
    # Retourner les données imputées ou modifiées pour les autres modules
    return(list(
      data = final_data,
      target_variable = reactive({ input$target }),  # Variable cible sélectionnée
      explanatory_variables = reactive({ input$explanatory })  # Variables explicatives sélectionnées
    ))
  })
}
