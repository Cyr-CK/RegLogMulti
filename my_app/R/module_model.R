# R/module_model.R
module_model_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Model Section"),
    textInput(ns("model_name"), "Model Name"),
    actionButton(ns("run_model"), "Run Model"),
    verbatimTextOutput(ns("model_output"))
  )
}

module_model_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$model_output <- renderPrint({
      paste("Running model:", input$model_name)
    })
  })
}
