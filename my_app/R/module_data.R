# R/module_data.R
library(readxl)
library(DT)

module_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Data Section"),
    fileInput(ns("file"), "Upload Data (.csv or .xlsx)"),
    checkboxInput(ns("header"), "Display Header", value = TRUE),
    conditionalPanel(
      condition = "output.fileType == 'csv'",
      ns = ns,
      radioButtons(ns("sep"), "Separator",
                   choices = c("Comma" = ",",
                               "Semicolon" = ";",
                               "Tab" = "\t"),
                   selected = ",")
    ),
    DTOutput(ns("data_table"))
  )
}

module_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Detect file type for conditional UI
    output$fileType <- reactive({
      req(input$file)
      tools::file_ext(input$file$name)
    })
    outputOptions(output, "fileType", suspendWhenHidden = FALSE)
    
    # Read data based on file type and user input
    data <- reactive({
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      
      if (ext == "csv") {
        read.csv(input$file$datapath,
                 header = input$header,
                 sep = input$sep)
      } else if (ext == "xlsx") {
        read_excel(input$file$datapath,
                   col_names = input$header)
      } else {
        showNotification("Unsupported file type. Please upload a .csv or .xlsx file.", type = "error")
        return(NULL)
      }
    })
    
    # Render data as an interactive table
    output$data_table <- renderDT({
      req(data())
      datatable(data(), options = list(pageLength = 10, autoWidth = TRUE))
    })
  })
}
