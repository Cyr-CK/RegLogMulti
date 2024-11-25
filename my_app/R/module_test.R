# R/module_test.R
module_test_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Test Module"),
    actionButton(ns("test_button"), "Test"),
    h4("Result"),
    verbatimTextOutput(ns("test_result"))
  )
}
module_test_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Réagir lorsque le bouton est cliqué
    result <- eventReactive(input$test_button, {
      paste("Test executed at", Sys.time())
    })
    
    # Afficher le résultat
    output$test_result <- renderText({
      req(result())
      result()
    })
  })
}
