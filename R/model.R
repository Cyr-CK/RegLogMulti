
if (!require(R6)) {
    install.packages("R6")
    library(R6)
}
if (!require(caret)) {
    install.packages("caret")
    library(caret)
}
if (!require(Matrix)) {
    install.packages("Matrix")
    library(Matrix)
}
if (!require(matrixStats)) {
    install.packages("matrixStats")
    library(matrixStats)
}
library(R6)
library(caret)
library(Matrix)
library(matrixStats) # Charger le package matrixStats pour des opérations optimisées

#' MultinomialLogisticRegression
#' @description 
#' This class is used to perform one-hot encoding on categorical variables in a dataset.
#' 
MultinomialLogisticRegression <- R6::R6Class("MultinomialLogisticRegression",
  public = list(
    #' @field weights Matrix of weights used by the model for prediction.
    weights = NULL,
    #' @field classes Vector of unique class labels derived from the training data.
    classes = NULL,
    #' @field loss_history Numeric vector storing the loss values over iterations during training.
    loss_history = NULL,
    #' @field learning_rate_history Numeric vector storing the learning rate values over epochs.
    learning_rate_history = NULL,
    #' @field learning_rate Numeric value indicating the learning rate used for optimization.
    learning_rate = 0.01,
    #' @field optimizer String specifying the optimization method (e.g., "adam", "sgd").
    optimizer = "adam",
    #' @field regularization_strength Numeric value for the strength of the regularization term.
    regularization = NULL,
    #' @field regularization_strength Numeric value for the strength of the regularization term.
    regularization_strength = 0.01,
    #' @field lr_decay Logical value indicating whether learning rate decay is applied.
    lr_decay = FALSE,
    #' @field decay_rate Numeric value for the learning rate decay factor.
    decay_rate = 0.001,
    #' @field loss_function String specifying the loss function to use ("cross_entropy" or "squared_error").
    loss_function = "cross_entropy",
    #' @field epsilon Numeric value to prevent division by zero in optimization calculations.
    epsilon = 1e-8,
    #' @field batch_size Integer specifying the number of samples per mini-batch for training.
    batch_size = 32,
    #' @field beta1 Numeric value for the exponential decay rate for the first moment in the Adam optimizer.
    beta1 = 0.9,
    #' @field beta2 Numeric value for the exponential decay rate for the second moment in the Adam optimizer.
    beta2 = 0.999,
    #' @field m Matrix used for storing the first moment estimates in the Adam optimizer.
    m = NULL,
    #' @field v Matrix used for storing the second moment estimates in the Adam optimizer.
    v = NULL,
    #' @field t Integer representing the current iteration for the Adam optimizer.
    t = 0,
    #' @field iterations Integer specifying the total number of iterations for training.
    iterations = 100,  # Nombre d'itérations
    #' @field warmup_epochs Integer specifying the number of warmup epochs where the learning rate is gradually increased.
    warmup_epochs = 0,  # Nombre d'épochs de warmup
    #' @field initial_learning_rate Numeric value for the initial learning rate during the warmup phase.
    initial_learning_rate = 0.001,  # Learning rate initial pour le warmup
    #' @field initialization String specifying the method used for initializing weights ("xavier" or "zeros").
    initialization = NULL,  # Initialisation des poids

    #' @description
    #' This method initializes a model with all defaults paramaters.
    #' 
    #' @param learning_rate Numeric. The learning rate used for optimization.
    #' @param iterations Integer. The total number of iterations for training.
    #' @param optimizer String. The optimization algorithm to use (e.g., "adam", "sgd").
    #' @param regularization String. The type of regularization to apply ("L1", "L2", or NULL).
    #' @param regularization_strength Numeric. The strength of the regularization term.
    #' @param lr_decay Logical. Whether to apply learning rate decay.
    #' @param decay_rate Numeric. The decay rate factor for learning rate decay.
    #' @param loss_function String. The loss function to use ("cross_entropy" or "squared_error").
    #' @param batch_size Integer. The number of samples per mini-batch for training.
    #' @param beta1 Numeric. The exponential decay rate for the first moment in the optimizer.
    #' @param beta2 Numeric. The exponential decay rate for the second moment in the optimizer.
    #' @param epsilon Numeric. A small value to prevent division by zero in optimization calculations.
    #' @param warmup_epochs Integer. The number of warmup epochs during which the learning rate is gradually increased.
    #' @param initial_learning_rate Numeric. The initial learning rate for the warmup phase.
    #' @param initialization String. The method used for initializing weights ("xavier" or "zeros").
    #' 
    #' @return Initializes an instance of the `MultinomialLogisticRegression` class with the specified parameters.
    #' @exampless
    #' # Create a new instance of the class
    #' model <- MultinomialLogisticRegression$new(learning_rate = 0.001, 
    #'  #                                            iterations = 100, 
    #'  #                                            batch_size =4, 
    #'  #                                            regularization = "L1", 
    #'  #                                            regularization_strength = 0.01, 
    #'  #                                            optimizer = "adam",
    #'  #                                            beta1 = 0.9,
    #'  #                                            beta2 = 0.999,
    #'  #                                            epsilon = 1e-8,
    #'  #                                            lr_decay = TRUE,        
    #'  #                                            decay_rate = 0.01,
    #'  #                                            warmup_epochs = 10, 
    #'  #                                            initial_learning_rate = 0.0001,
    #'  #                                            initialization = "zeros")

    initialize = function(learning_rate = 0.01, iterations = 100, optimizer = "adam",
                          regularization = NULL, regularization_strength = 0.01,
                          lr_decay = FALSE, decay_rate = 0.001,
                          loss_function = "cross_entropy",
                          batch_size = 32,
                          beta1 = 0.9, beta2 = 0.999, epsilon = 1e-8,
                          warmup_epochs = 0, initial_learning_rate = 0.001, initialization = "xavier") {
      self$learning_rate <- learning_rate # Taux d'apprentissage
      self$optimizer <- optimizer # Optimiseur
      self$regularization <- regularization # Type de régularisation
      self$regularization_strength <- regularization_strength # Force de régularisation
      self$lr_decay <- lr_decay # Décroissance du taux d'apprentissage  
      self$decay_rate <- decay_rate # Taux de décroissance du taux d'apprentissage
      self$loss_function <- loss_function # Fonction de perte
      self$loss_history <- numeric(iterations)  # Pré-allocation
      self$learning_rate_history <- numeric(iterations)  # Pré-allocation
      self$batch_size <- batch_size # Taille du mini-batch  
      self$beta1 <- beta1 # Taux de décroissance du premier moment
      self$beta2 <- beta2 # Taux de décroissance du deuxième moment
      self$epsilon <- epsilon # Valeur epsilon pour éviter la division par zéro
      self$iterations <- iterations # Nombre d'itérations
      self$warmup_epochs <- warmup_epochs # Nombre d'époques de warmup
      self$initial_learning_rate <- initial_learning_rate # Taux d'apprentissage initial
      self$initialization <- initialization # Initialisation des poids
    },
    #' @description 
    #' This method fits the model to data given.
    #' @param X Matrix. Explanatory variables
    #' @param y Factor. Target Variable
    #' @param epochs Integer. Iteration for convergence
    #' @param progress_callback Function. To view the progress of model training on data.
    #' @return Nothing. The object is internally updated when using this method.
    #' @exampless
    #'  # model$fit(as.matrix(X_train), y_train)

    fit = function(X, y, epochs = self$iterations , progress_callback = NULL) {
      if (!is.matrix(X)) {
        stop("`X` doit être une matrice.")
      }
      if (!is.factor(y)) {
        y <- as.factor(y)
      }

      self$classes <- levels(y)
      n_classes <- length(self$classes)
      n_features <- ncol(X)
      n_samples <- nrow(X)
       # Initialize weights
    if (self$initialization == "zeros") {
        self$weights <- matrix(0, nrow = n_features, ncol = n_classes)
    } else if (self$initialization == "xavier") {
        limit <- sqrt(6 / (n_features + n_classes))
        self$weights <- matrix(runif(n_features * n_classes, min = -limit, max = limit), 
                                nrow = n_features, ncol = n_classes)
    }


      if (self$optimizer == "adam") {
        self$m <- matrix(0, nrow = n_features, ncol = n_classes)
        self$v <- matrix(0, nrow = n_features, ncol = n_classes)
        self$t <- 0
      }

      pb <- txtProgressBar(min = 0, max = epochs, style = 3)

      # Pré-allocation des variables intermédiaires
      current_lr <- self$learning_rate
      epoch_losses <- numeric(ceiling(nrow(X)/self$batch_size))
      y_one_hot <- matrix(0, nrow = n_samples, ncol = n_classes)
            for (i in 1:n_samples) {
                class_index <- which(self$classes == y[i])
                y_one_hot[i, class_index] <- 1
            }


      for (epoch in 1:epochs) {
        if (is.null(self$batch_size)) {
                    batch_indices <- list(1:n_samples)
                    
        } else {
             shuffled_indices <- sample(1:n_samples)  # Mélanger les indices
            batch_indices <- split(shuffled_indices, ceiling(seq_along(shuffled_indices) / self$batch_size))
            }
   epoch_loss <- 0  # Initialiser la perte cumulative pour l'époque
    total_batches <- length(batch_indices)  # Nombre de mini-batchs dans une époque
    if (self$warmup_epochs > 0 && epoch <= self$warmup_epochs) {
      current_lr <- self$initial_learning_rate + 
                    (self$learning_rate - self$initial_learning_rate) * (epoch / self$warmup_epochs)
    } else {
      current_lr <- self$learning_rate
      if (self$lr_decay && epoch > self$warmup_epochs) {
        current_lr <- self$learning_rate / (1 + self$decay_rate * (epoch - self$warmup_epochs))
      }
    }

for (batch in batch_indices) {
    X_batch <- X[batch, , drop = FALSE]
    y_batch <- y_one_hot[batch, , drop = FALSE]

    scores <- X_batch %*% self$weights
    probs <- self$softmax(scores)

    # Calcul de la perte pour le mini-batch
    batch_loss <- self$compute_loss(probs, y_batch)

    # Mise à jour des poids via le gradient
    gradient <- t(X_batch) %*% (probs - y_batch) / length(batch)
    
    # Ajouter la régularisation si nécessaire
    if (!is.null(self$regularization)) {
        if (self$regularization == "L2") {
            gradient <- gradient + self$regularization_strength * self$weights
        } else if (self$regularization == "L1") {
            gradient <- gradient + self$regularization_strength * sign(self$weights)
        }
    }
    
    # Mise à jour des poids selon l'optimiseur
    if (self$optimizer == "adam") {
        self$t <- self$t + 1
        self$m <- self$beta1 * self$m + (1 - self$beta1) * gradient
        self$v <- self$beta2 * self$v + (1 - self$beta2) * (gradient^2)
        m_hat <- self$m / (1 - self$beta1^self$t)
        v_hat <- self$v / (1 - self$beta2^self$t)
        self$weights <- self$weights - current_lr * m_hat / (sqrt(v_hat) + self$epsilon)
    } else if (self$optimizer == "sgd") {
        self$weights <- self$weights - current_lr * gradient
    }

            # Ajouter la perte du mini-batch à la perte cumulative
            epoch_loss <- epoch_loss + batch_loss
        }

        # Calcul de la perte moyenne pour l'époque
        self$loss_history[epoch] <- epoch_loss / total_batches
        self$learning_rate_history[epoch] <- current_lr
        # Call the progress callback if provided
        if (!is.null(progress_callback)) {
          progress_callback(epoch, epochs, self$loss_history[epoch])
        }
        setTxtProgressBar(pb, epoch)
        }
        close(pb)
    },

    #' @description
    #' This function calculates the class predicted.
    #' @param X Matrix. Explanatory variables from the test dataset
    #' @return Vector. The predicted class for each value of the test dataset.
    #' 
    #' @exampless
    #' # predictions <- model$predict(X_test)

    predict = function(X) {
      if (!is.matrix(X)) {
        X <- as.matrix(X)
      }
      scores <- X %*% self$weights
      probs <- self$softmax(scores)
      max_indices <- max.col(probs)
      self$classes[max_indices]
    },

    #' @description
    #' This function calculates the probabilites of belonging to each class of the target variable.
    #' @param X Matrix. Explanatory variables from the test dataset.
    #' @return Matrix. The probability of belonging to each class.
    #' 
    #' @examples
    #' # predictions <- model$predict_proba(X_test)
    predict_proba = function(X) {
      if (!is.matrix(X)) {
        X <- as.matrix(X)
      }
      scores <- X %*% self$weights
      probs <- self$softmax(scores)
      return(probs)
    },
    #' @description
    #' This function computes the softmax probabilities for a given set of scores.
    #' @param scores Matrix. Matrix product between weights and the matrix with the explanatory variable.
    #' @return Matrix. Probabilities where each row  sums to 1.
    #' @examples
    #' # probs <- softmax(scores)
    softmax = function(scores) {
      exp_scores <- exp(scores - matrixStats::rowMaxs(scores))
      probs <- exp_scores / matrixStats::rowSums2(exp_scores)
      return(probs)
    },

    #' @description
    #' This function computes the loss value for a given set of probabilities and one-hot encoded target values.
    #' @param probs Matrix. Probabilities of belonging to each class.
    #' @param y_one_hot Matrix. One-hot encoded target values.
    #' @return Float. The loss value calculated based on the probabilities and target values.
    #' @examples
    #' # loss <- compute_loss(probs, y_one_hot)
    compute_loss = function(probs, y_one_hot) {
      if (self$loss_function == "cross_entropy") {
        epsilon_val <- 1e-15
        probs_clipped <- pmax(pmin(probs, 1 - epsilon_val), epsilon_val)
        -mean(matrixStats::rowSums2(y_one_hot * log(probs_clipped)))
      } else if (self$loss_function == "squared_error") {
        mean((probs - y_one_hot)^2)
      } else {
        stop("Fonction de perte inconnue.")
      }
    },

    #' @description
    #' This function returns the loss history of the model during training.
    #' @return Numeric vector. Loss values over iterations during training.
    #' @examples
    #' # loss_history <- model$get_loss_history()
    get_loss_history = function() {
      self$loss_history
    },

    #' @description
    #' To plot the loss curve
    #' @return lineplot. Loss variations as a function of iteration. 
    #' @examples
    #' #  model$plot_loss_history()
    plot_loss_history = function() {
      if (length(self$loss_history) == 0) {
        plot.new()
        title("No loss history available.")
      } else {
        plot(self$loss_history, type = "l", col = "blue",
             main = "Loss History",
             xlab = "Iteration",
             ylab = "Loss")
      }
    },
    #' @description
    #' To plot the learning rate by epoch
    #' @return lineplot. Learning rate by epoch.
    #' @examples
    #' #  model$plot_loss_history()
    plot_learning_rate_history = function() {
      if (length(self$learning_rate_history) == 0) {
        plot.new()
        title("No learning rate history available.")
      } else {
        plot(self$learning_rate_history, type = "l", col = "green",
             main = "Learning Rate History",
             xlab = "Epoch",
             ylab = "Learning Rate")
      }
    },

    #' @description
    #' To have the importance of each variable
    #' @return DataFrame with the importance calculated for each variable.
    #' @examples
    #' #  model$get_variable_importance()
    get_variable_importance = function() {
      if (is.null(self$weights)) {
        stop("Le modèle n'est pas encore entraîné.")
      }
      importance <- rowMeans(abs(self$weights))
      importance_df <- data.frame(Variable = rownames(self$weights), Importance = importance)
      importance_df <- importance_df[order(-importance_df$Importance), ]
      return(importance_df)
    },

    #' @description
    #' To have the N most important variable in the model
    #' @param n Integer. Number of the best variable to select
    #' @return Vector. Names of the n best variable based on their importance.
    #' @examples
    #' #  model$var_select(5)
    var_select = function(n) {
      importance_df <- self$get_variable_importance()
      top_vars <- importance_df$Variable[1:n]
      return(top_vars)
    },

    #' @description
    #' Calculate the accuracy of the model
    #' @param y_test Factor. The target variable with the real class.
    #' @param y_pred Factor. The class predicted from the model
    #' @return Float. The accuracy of the model.
    #' @examples
    #' #  model$accuracy(y_test, y_pred)

    accuracy = function(y_test, y_pred) {
      mean(y_test == y_pred)
    },

    #' @description
    #' Calculate the confusion matrix between the prediction of the model and the real class.
    #' @param y_test Factor. The target variable with the real class.
    #' @param y_pred Factor. The class predicted from the model
    #' @return Matrix. Display the confusion matrix calculated.
    #' @examples
    #' #  model$confusion_matrix(y_test, y_pred)
    confusion_matrix = function(y_test, predictions) {
            
        if (!is.factor(y_test)) {
            y_test <- as.factor(y_test)
        }
        # Afficher la matrice de confusion
        
        predictions <- factor(predictions, levels = levels(y_test))
        confusion_matrix <- confusionMatrix(predictions,y_test)
        print("Confusion Matrix:")
        print(confusion_matrix$table)
    },
    #' @description
    #' Display all informations parameters about the models
    #' @return Text. print informations
    #' @examples
    #' #  model$summary()
    summary = function() {
      cat("Multinomial Logistic Regression Model\n")
      cat("-------------------------------\n")
      cat("Learning Rate: ", self$learning_rate, "\n")
      cat("Optimizer: ", self$optimizer, "\n")
      cat("Regularization: ", self$regularization, "\n")
      cat("Regularization Strength: ", self$regularization_strength, "\n")
      cat("Loss Function: ", self$loss_function, "\n")
      cat("Batch Size: ", self$batch_size, "\n")
      cat("Iterations: ", self$iterations, "\n")
      cat("Warmup Epochs: ", self$warmup_epochs, "\n")
      cat("Initial Learning Rate: ", self$initial_learning_rate, "\n")
      cat("Initialization: ", self$initialization, "\n")
    },
    #' @description
    #' display a small summary of the model
    #' @return Text. print informations
    #' @examples
    #' #  model$print()
    print = function() {
        cat("Multinomial Logistic Regression Model\n")
        cat("-------------------------------\n")
        cat("Learning Rate: ", self$learning_rate, "\n")
        cat("Iterations: ", self$iterations, "\n")
        cat("Batch Size: ", self$batch_size, "\n")
    }
  )
)
    #' @description
    #' To split the dataset with train/test method
    #' @param data Dataframe with all the data without any modfications.
    #' @param target_var String or List with the target variable
    #' @param explanatory_vars Matrix with all the explanatory variable
    #' @param train_ratio Float. Percentage to be passed in the sample train.
    #' @param random_stat Integer. Controls the random seed for reproducibility.
    #' @param shuffle binary. If true, then the samples are constructed with randomness.
    #' @return List with data separated into trains/tests 
    #' @examples
    #' # split_data <- split_train_test(data = data, target_var = target_var, explanatory_vars = explanatory_vars, train_ratio = 0.9, random_state = 123, 
split_train_test <- function(data, target_var, explanatory_vars, train_ratio = 0.8, random_state = NULL, shuffle = TRUE) {
    # Vérifier les entrées
    if (!is.data.frame(data)) {
        stop("`data` doit être un data.frame.")
    }
    if (!(target_var %in% names(data))) {
        stop(paste("La variable cible", target_var, "n'existe pas dans les données."))
    }
    if (!all(explanatory_vars %in% names(data))) {
        stop("Certaines variables explicatives n'existent pas dans les données.")
    }
    if (!is.numeric(train_ratio) || train_ratio <= 0 || train_ratio >= 1) {
        stop("`train_ratio` doit être un nombre entre 0 et 1.")
    }
    if (!is.logical(shuffle)) {
        stop("`shuffle` doit être une valeur logique (TRUE ou FALSE).")
    }
    
    # Définir la graine aléatoire si spécifiée
    if (!is.null(random_state)) {
        set.seed(random_state)
    }
    
    if (shuffle) {
        # Mélanger les indices des lignes si shuffle est TRUE
        shuffled_indices <- sample(nrow(data))
    } else {
        # Utiliser les indices dans l'ordre original si shuffle est FALSE
        shuffled_indices <- 1:nrow(data)
    }
    
    # Calculer le nombre d'échantillons d'entraînement
    train_size <- floor(train_ratio * nrow(data))
    
    # Séparer les indices en ensembles d'entraînement et de test
    train_indices <- shuffled_indices[1:train_size]
    test_indices <- shuffled_indices[(train_size + 1):nrow(data)]
    
    # Créer les ensembles d'entraînement et de test
    X_train <- data[train_indices, explanatory_vars, drop = FALSE]
    X_test <- data[test_indices, explanatory_vars, drop = FALSE]
    y_train <- data[train_indices, target_var]
    y_test <- data[test_indices, target_var]
    
    # Retourner les ensembles divisés
    return(list(
        X_train = X_train,
        X_test = X_test,
        y_train = y_train,
        y_test = y_test
    ))
}
