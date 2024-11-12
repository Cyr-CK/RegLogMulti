# R/module_training.R
module_training_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Training Section"),
    sliderInput(ns("epochs"), "Number of Epochs", min = 1, max = 100, value = 10),
    actionButton(ns("train"), "Start Training"),
    verbatimTextOutput(ns("training_output"))
  )
}

module_training_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$training_output <- renderPrint({
      paste("Training for", input$epochs, "epochs")
    })
  })
}
