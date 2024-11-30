# R/module_preprocessing.R

module_preprocessing_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Preprocessing Configuration"),
    tabsetPanel(
      tabPanel(
        "Qualitative Variables",
        h4("One-Hot Encoding"),
        actionButton(ns("one_hot_encode_btn"), "Apply One-Hot Encoding"),
        h4("Encoded Data"),
        DTOutput(ns("encoded_data"))
      ),
      tabPanel(
        "Quantitative Variables",
        h4("Transformations"),
        selectInput(ns("quantitative_transform"), "Select Transformation:",
                    choices = c("Standard Scaler", "Min-Max Scaler", "Robust Scaler"),
                    selected = "Standard Scaler"),
        actionButton(ns("apply_quantitative_transform"), "Apply Transformation"),
        h4("Transformed Data"),
        DTOutput(ns("transformed_data"))
      ),
      tabPanel(
        "Mixed Variables",
        h4("FAMD (Factor Analysis of Mixed Data)"),
        actionButton(ns("apply_famd"), "Apply FAMD"),
        h4("FAMD Results"),
        DTOutput(ns("famd_results"))
      )
    )
  )
}

module_preprocessing_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Réactif pour accéder aux données transmises
    raw_data <- reactive({
      req(data())
      data()
    })
    
    # Placeholder pour stocker les données finales après traitement
    processed_data <- reactiveVal(NULL)
    
    # Placeholders pour stocker les résultats intermédiaires
    encoded_data <- reactiveVal(NULL)
    transformed_data <- reactiveVal(NULL)
    famd_results <- reactiveVal(NULL)
    
    ## --- Variables qualitatives : One-Hot Encoding ---
    observeEvent(input$one_hot_encode_btn, {
      req(raw_data())
      data <- raw_data()
      
      # Identifier les colonnes qualitatives
      qualitative_cols <- names(data)[sapply(data, is.character) | sapply(data, is.factor)]
      
      if (length(qualitative_cols) == 0) {
        # Aucune colonne qualitative trouvée
        showNotification("No qualitative variables found for One-Hot Encoding.", type = "error")
        return()
      }
      
      # Appliquer l'encodage uniquement sur les colonnes qualitatives
      tryCatch({
        ohe <- One.Hot.Encoder$new()
        encoded <- ohe$fit_transform(data[qualitative_cols])
        
        # Ajouter les colonnes restantes (non qualitatives)
        non_qual_cols <- setdiff(names(data), qualitative_cols)
        final_data <- cbind(data[non_qual_cols], encoded)
        
        # Mettre à jour les données encodées et finales
        encoded_data(final_data)
        processed_data(final_data)
      }, error = function(e) {
        showNotification(paste("Error during One-Hot Encoding:", e$message), type = "error")
      })
    })
    
    output$encoded_data <- renderDT({
      req(encoded_data())
      datatable(encoded_data(), options = list(pageLength = 10, autoWidth = TRUE))
    })
    
    ## --- Variables quantitatives : Transformation ---
    observeEvent(input$apply_quantitative_transform, {
      req(raw_data())
      data <- raw_data()
      
      # Détecter les colonnes quantitatives
      quantitative_cols <- names(data)[sapply(data, is.numeric)]
      if (length(quantitative_cols) == 0) {
        showNotification("No quantitative variables found.", type = "error")
        return()
      }
      
      # Sélection des colonnes quantitatives
      quantitative_data <- data[quantitative_cols]
      
      # Appliquer la transformation choisie
      transformation <- input$quantitative_transform
      tryCatch({
        if (transformation == "Standard Scaler") {
          scaler <- Standard.Scaler$new()
          transformed <- scaler$fit_transform(quantitative_data)
        } else if (transformation == "Min-Max Scaler") {
          scaler <- MinMax.Scaler$new()
          transformed <- scaler$fit_transform(quantitative_data)
        } else if (transformation == "Robust Scaler") {
          scaler <- Robust.Scaler$new()
          transformed <- scaler$fit_transform(quantitative_data)
        }
        
        # Mettre à jour les données transformées et finales
        data[quantitative_cols] <- transformed
        transformed_data(as.data.frame(data))
        processed_data(as.data.frame(data))
      }, error = function(e) {
        showNotification(paste("Error during quantitative transformation:", e$message), type = "error")
      })
    })
    
    output$transformed_data <- renderDT({
      req(transformed_data())
      datatable(transformed_data(), options = list(pageLength = 10, autoWidth = TRUE))
    })
    
    ## --- Variables mixtes : FAMD ---
    observeEvent(input$apply_famd, {
      req(raw_data())
      data <- raw_data()
      
      tryCatch({
        # Charger la classe FAMD
        famd <- FAMD_$new()  # Instancier la classe FAMD
        famd_result <- famd$fit_transform(data)
        
        # Stocker les résultats FAMD
        famd_results(as.data.frame(famd_result))
        processed_data(as.data.frame(famd_result))
      }, error = function(e) {
        showNotification(paste("Error during FAMD:", e$message), type = "error")
      })
    })
    
    output$famd_results <- renderDT({
      req(famd_results())
      datatable(famd_results(), options = list(pageLength = 10, autoWidth = TRUE))
    })
    
    # Retourner les données finales pour le module suivant
    return(list(data = reactive(processed_data)))
  })
}
