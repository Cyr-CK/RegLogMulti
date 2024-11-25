# R/module_model.R
module_model_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Model Configuration"),
    fluidRow(
      column(6,
             textInput(ns("text1"), "Input Text 1", placeholder = "Enter text here"),
             textInput(ns("text2"), "Input Text 2", placeholder = "Enter another text")
      ),
      column(6,
             selectInput(ns("dropdown1"), "Dropdown 1", 
                         choices = c("Option 1", "Option 2", "Option 3"), 
                         selected = "Option 1"),
             selectInput(ns("dropdown2"), "Dropdown 2", 
                         choices = c("Choice A", "Choice B", "Choice C"), 
                         selected = "Choice A")
      )
    ),
    numericInput(ns("numeric_input"), 
                 "Alpha (Laplace smoothing parameter)", 
                 value = 1, min = 0, max = 10, step = 0.1),
    actionButton(ns("submit"), "Submit"),
    verbatimTextOutput(ns("form_output"))
  )
}
module_model_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Collect form data and display it
    output$form_output <- renderPrint({
      req(input$submit)
      list(
        Text1 = input$text1,
        Text2 = input$text2,
        Dropdown1 = input$dropdown1,
        Dropdown2 = input$dropdown2,
        Alpha = input$numeric_input
      )
    })
  })
}