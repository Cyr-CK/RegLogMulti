
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
if (!requireNamespace("shinyjs", quietly = TRUE)) {
  install.packages("shinyjs")
}
if (!requireNamespace("DT", quietly = TRUE)) {
  install.packages("DT")
}
if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table")
}
if (!requireNamespace("R6", quietly = TRUE)) {
  install.packages("R6")
}
if (!requireNamespace("magrittr", quietly = TRUE)) {
  install.packages("magrittr")
}
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}

library(shiny)
library(magrittr)
library(R6)
library(DT)
library(data.table)


library(shinyjs)
# Définir l'opérateur %||% si ce n'est pas déjà défini
`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

module_training_ui <- function(id) {
  ns <- NS(id)
    fluidPage(
          useShinyjs(), # Utiliser la bibliothèque shinyjs

  tagList(
    h3("Model Training"),
    sidebarLayout(
      sidebarPanel(
        h4("Model Parameters"),
        selectInput(
          ns("initialization"),
          "Weight Initialization:",
          choices = c("zeros", "xavier"),
          selected = "xavier"
        ),
        numericInput(
          ns("learning_rate"),
          "Learning Rate:",
          value = 0.01,
          min = 0.00001,
          step = 0.001
        ),
        numericInput(
          ns("batch_size"),
          "Batch Size:",
          value = 32,
          min = 1,
          step = 1
        ),
        selectInput(
          ns("optimizer"),
          "Optimizer:",
          choices = c("adam", "sgd"),
          selected = "adam"
        ),
        numericInput(
              ns("warmup_epochs"),
              "Number of Warmup Epochs:",
              value = 10,
              min = 0,
              step = 1
            ),
        checkboxInput(
          ns("lr_decay"),
          "Enable Learning Rate Decay",
          value = FALSE
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("lr_decay")),
          numericInput(
            ns("decay_rate"),
            "Decay Rate:",
            value = 0.001,
            min = 0,
            step = 0.0001
          )
        ),
        selectInput(
          ns("regularization"),
          "Regularization:",
          choices = c("none", "L1", "L2"),
          selected = "none"
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] != 'none'", ns("regularization")),
          numericInput(
            ns("regularization_strength"),
            "Regularization Strength:",
            value = 0.01,
            min = 0,
            step = 0.01
          )
        ),
        numericInput(
          ns("iterations"),
          "Number of Iterations:",
          value = 1000,
          min = 100,
          step = 100
        ),
        actionButton(
          ns("fit_model"),
          "Fit Model",
          icon = icon("play"),
          class = "btn-primary"
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Training",
                   verbatimTextOutput(ns("training_status")
                                      ),
              h4("Training Progress"),
                    tags$div(
        style = "width: 100%; background-color: #f3f3f3; border: 1px solid #ccc;",
        tags$div(
          id = "trainingProgressBar",
          style = "width: 0%; height: 20px; background-color: #4caf50;"
      
      
        )
                   
                   ),
         
                              # Outputs for Accuracy and Confusion Matrix
      uiOutput(ns("training_results"))
          ),


          tabPanel("Loss Curve",
                   plotOutput(ns("loss_plot"))
       ),
       tabPanel("Lr",
                   plotOutput(ns("lr_plot"))
      
        ),
        tabPanel("Variable Importance",
                     h4("Top 10 Important Variables"),
                     plotOutput(ns("var_importance_plot"))
      )
    )
  )
  )))
}

module_training_server <- function(id, data , target_variable, explanatory_variables) {
  moduleServer(id, function(input, output, session) {
   ns <- session$ns
  observe({
    runjs("document.getElementById('trainingProgressBar').style.width = '0%';")
  })
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


    # Initialiser shinyjs pour permettre l'activation/désactivation des boutons
    disable("test_model")  # Désactiver le bouton Test Model au démarrage

    # Reactive values to store the model and its history
    rv <- reactiveValues(
      model = NULL,
      loss_history = NULL,
      performance = NULL,
      confusion_matrix = NULL,
      split_data = NULL,
      X_train = NULL,
      X_test = NULL,
      y_train = NULL,
      y_test = NULL,
      predictions = NULL,
      learning_rate_history = NULL ,
      var_importance = NULL


    )

    # Observe Fit Model button
    observeEvent(input$fit_model, {
      # Appeler les fonctions réactives pour obtenir les valeurs réelles
      req(processed_data())
      req(reactive_target())
      req(reactive_explanatory())

      disable("fit_model")
      
      # Afficher une notification de démarrage
      showNotification("Training started...", type = "message")

      # Initialiser le modèle avec les paramètres définis par l'utilisateur
      rv$model <- MultinomialLogisticRegression$new(
        initialization = input$initialization,
        lr_decay = input$lr_decay,
        decay_rate = input$decay_rate,
        iterations = input$iterations,
        optimizer = input$optimizer,
        regularization = input$regularization,
        regularization_strength = input$regularization_strength,
        batch_size = input$batch_size,
        learning_rate = input$learning_rate,
        warmup_epochs = input$warmup_epochs

      )

      # Extraire les données de formation en utilisant les valeurs réactives
      
      #enlever la target variable de exploratory variables si elle est dedans
      if(reactive_target() %in% reactive_explanatory()){
        reactive_explanatory_bis <- reactive_explanatory()[-which(reactive_explanatory() == reactive_target())]
      }


      


      split_data <- split_train_test(
        data = processed_data(),
        target_var = reactive_target(),
        explanatory_vars = reactive_explanatory_bis,
        train_ratio = 0.9,
        random_state = 123
      )

      rv$X_train <- split_data$X_train
      rv$y_train <- split_data$y_train
      rv$X_test <- split_data$X_test
      rv$y_test <- split_data$y_test

      # Fit le modèle
      tryCatch({


        # Fit the model
        rv$model$fit(as.matrix(rv$X_train), rv$y_train, progress_callback = function(current, total, loss) {
      percent <- (current / total) * 100
     percent <- (current / total) * 100
      runjs(sprintf("document.getElementById('trainingProgressBar').style.width = '%0.2f%%';", percent))
    })
        rv$loss_history <- rv$model$loss_history
        rv$learning_rate_history <- rv$model$learning_rate_history
        showNotification("Model training completed successfully.", type = "message")
  

        # Activer le bouton Test Model après un entraînement réussi
        enable("test_model")
        enable("fit_model")
      }, error = function(e) {
        showNotification(paste("Error during model training:", e$message), type = "error")
      })
      
      rv$predictions = rv$model$predict(as.matrix(rv$X_test))
    # After training completes
     # Retrieve accuracy and confusion matrix
        accuracy <- rv$model$accuracy(rv$y_test ,rv$predictions )
        confusion_matrix <- rv$model$confusion_matrix(rv$y_test ,rv$predictions )
      
  # Convert confusion matrix to data frame with row names
        confusion_matrix_df <- as.data.frame(confusion_matrix)
        confusion_matrix_df <- cbind(Prediction = rownames(confusion_matrix_df), confusion_matrix_df)
        rownames(confusion_matrix_df) <- NULL
        
      var_importance <- rv$model$get_variable_importance()
      top_vars <- head(var_importance[order(-var_importance$Importance), ], 10)
      rv$var_importance <- top_vars

        output$training_status <- renderPrint({
        rv$model$summary()
      })
        # Render training results
        output$training_results <- renderUI({
          tagList(
            h4("Model Accuracy"),
            verbatimTextOutput(ns("accuracy_output")),
            
            h4("Confusion Matrix"),
            tableOutput(ns("confusion_matrix_output"))
          )
        })
        
        output$accuracy_output <- renderText({
          paste("Accuracy:", round(accuracy, 4))
        })
        
        output$confusion_matrix_output <- renderTable({
          confusion_matrix_df
        }, rownames = FALSE)
        

    output$loss_plot <- renderPlot({
      req(rv$loss_history)
      if(length(rv$loss_history) == 0){
        plot.new()
        title("No loss history available.")
      } else {
        plot(rv$loss_history, type = "l", col = "blue",
             main = "Loss Curve",
             xlab = "Iteration",
             ylab = "Loss")
      }
    })
     # Render Learning Rate Plot
    output$lr_plot <- renderPlot({
      req(rv$learning_rate_history)
      if(length(rv$learning_rate_history) == 0){
        plot.new()
        title("No learning rate history available.")
      } else {
        plot(rv$learning_rate_history, type = "l", col = "green",
             main = "Learning Rate Curve",
             xlab = "Epoch",
             ylab = "Learning Rate")
      }
    })

   
# Render Variable Importance Plot
      output$var_importance_plot <- renderPlot({
        ggplot(rv$var_importance, aes(x = reorder(Variable, Importance), y = Importance)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          coord_flip() +
          labs(title = "Top 10 Important Variables",
               x = "Variables",
               y = "Importance") +
          theme_minimal()
      })


    })
  })
}


