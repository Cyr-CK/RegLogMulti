
library(shiny)
library(magrittr)
library(R6)
library(DT)
library(data.table)
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
if (!requireNamespace("shinyjs", quietly = TRUE)) {
  install.packages("shinyjs")
}
library(shinyjs)
# Définir l'opérateur %||% si ce n'est pas déjà défini
`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

module_training_ui <- function(id) {
  ns <- NS(id)
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
          min = 0.0001,
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
            value = 1,
            min = 0,
            step = 0.1
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
        ),
        actionButton(
          ns("test_model"),
          "Test Model",
          icon = icon("search"),
          class = "btn-success",
          disabled = TRUE
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Training",
                   verbatimTextOutput(ns("training_status"))
          ),
          tabPanel("Loss Curve",
                   plotOutput(ns("loss_plot"))
       )
        )
      )
    )
  )
}

module_training_server <- function(id, data , target_variable, explanatory_variables) {
  moduleServer(id, function(input, output, session) {
   ns <- session$ns

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
    shinyjs::disable("test_model")  # Désactiver le bouton Test Model au démarrage

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
      y_test = NULL
    )

    # Observe Fit Model button
    observeEvent(input$fit_model, {
      # Appeler les fonctions réactives pour obtenir les valeurs réelles
      req(processed_data())
      req(reactive_target())
      req(reactive_explanatory())

      print("Model Training")

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
      )

      print("Model Initialized")
      print("target" )
      print(reactive_target())
      print("explanatory")
      print(reactive_explanatory())
      print("names")
      print(names(processed_data()))
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
      print("Data Split")
      rv$X_train <- split_data$X_train
      rv$y_train <- split_data$y_train
      rv$X_test <- split_data$X_test
      rv$y_test <- split_data$y_test

      # Afficher le modèle en cours dans la console R
      print("Current Model Initialized:")
      print(rv$model)

      # Fit le modèle
      tryCatch({
        print(str(rv$X_train))
        print(str(rv$y_train))

        # Fit the model
        rv$model$fit(as.matrix(rv$X_train), rv$y_train)
        rv$loss_history <- rv$model$loss_history
        showNotification("Model training completed successfully.", type = "message")
        
        # Afficher le modèle après l'entraînement
        print("Model after Training:")
        print(rv$model)
        print("Loss History:")
        print(rv$loss_history)

        # Activer le bouton Test Model après un entraînement réussi
        shinyjs::enable("test_model")
      }, error = function(e) {
        showNotification(paste("Error during model training:", e$message), type = "error")
      })
    })
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

    # Observe Test Model button
    observeEvent(input$test_model, {
      req(rv$model)
      req(data$data())
      req(target_variable())
      req(explanatory_variables())

      # Extraire les données de test en utilisant les valeurs réactives
      split_data <- split_train_test(
        data = data$data(), 
        target_var = target_variable(),
        explanatory_vars = explanatory_variables(),
        train_ratio = 0.9, 
        random_state = 123
      )
      X_test <- split_data$X_test
      y_test <- split_data$y_test

      # Faire des prédictions
      predictions <- rv$model$predict(X_test)

      # Calculer la précision
      accuracy <- mean(predictions == y_test)
      showNotification(paste("Model accuracy:", accuracy), type = "message")
    })
  })
}