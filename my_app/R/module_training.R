# module_training.R

module_training_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Training Configuration"),
    h4("Selected Variables"),
    verbatimTextOutput(ns("selected_variables")),
    h4("Processed Data Preview"),
    DTOutput(ns("processed_data_preview"))
  )
}

module_training_server <- function(id, data, target_variable, explanatory_variables) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Extraire les données prétraitées depuis l'entrée 'data'
    processed_data <- reactive({
      req(data())  # Exécute la fonction réactive
      df <- data()()  # Appelle la réactive retournée par 'data'
      if (!is.data.frame(df)) {
        stop("'data' must be a data.frame")
      }
      df
    })
    
    # Récupérer les variables cible et explicatives
    reactive_target <- reactive({
      req(target_variable())
      target_variable()
    })
    
    reactive_explanatory <- reactive({
      req(explanatory_variables())
      explanatory_variables()
    })
    
    # Affiche les variables sélectionnées
    output$selected_variables <- renderPrint({
      list(
        Target = reactive_target(),
        Explanatory = reactive_explanatory()
      )
    })
    
    # Affiche les données prétraitées
    output$processed_data_preview <- renderDT({
      req(processed_data())
      datatable(processed_data(), options = list(pageLength = 10, autoWidth = TRUE))
    })
  })
}
