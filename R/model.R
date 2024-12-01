
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


MultinomialLogisticRegression <- R6::R6Class("MultinomialLogisticRegression",
  public = list(
    weights = NULL,
    classes = NULL,
    loss_history = NULL,
    learning_rate_history = NULL,
    learning_rate = 0.01,
    optimizer = "adam",
    regularization = NULL,
    regularization_strength = 0.01,
    lr_decay = FALSE,
    decay_rate = 0.001,
    loss_function = "cross_entropy",
    epsilon = 1e-8,
    batch_size = 32,
    beta1 = 0.9,
    beta2 = 0.999,
    m = NULL,
    v = NULL,
    t = 0,
    iterations = 100,  # Nombre d'itérations
    warmup_epochs = 0,  # Nombre d'épochs de warmup
    initial_learning_rate = 0.001,  # Learning rate initial pour le warmup
    initialization = NULL,  # Initialisation des poids

    initialize = function(learning_rate = 0.01, iterations = 100, optimizer = "adam",
                          regularization = NULL, regularization_strength = 0.01,
                          lr_decay = FALSE, decay_rate = 0.001,
                          loss_function = "cross_entropy",
                          batch_size = 32,
                          beta1 = 0.9, beta2 = 0.999, epsilon = 1e-8,
                          warmup_epochs = 0, initial_learning_rate = 0.001, initialization = "xavier") {
      self$learning_rate <- learning_rate
      self$optimizer <- optimizer
      self$regularization <- regularization
      self$regularization_strength <- regularization_strength
      self$lr_decay <- lr_decay
      self$decay_rate <- decay_rate
      self$loss_function <- loss_function
      self$loss_history <- numeric(iterations)  # Pré-allocation
      self$learning_rate_history <- numeric(iterations)  # Pré-allocation
      self$batch_size <- batch_size
      self$beta1 <- beta1
      self$beta2 <- beta2
      self$epsilon <- epsilon
      self$iterations <- iterations
      self$warmup_epochs <- warmup_epochs
      self$initial_learning_rate <- initial_learning_rate
      self$initialization <- initialization
    },

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

    predict = function(X) {
      if (!is.matrix(X)) {
        X <- as.matrix(X)
      }
      scores <- X %*% self$weights
      probs <- self$softmax(scores)
      max_indices <- max.col(probs)
      self$classes[max_indices]
    },

    predict_proba = function(X) {
      if (!is.matrix(X)) {
        X <- as.matrix(X)
      }
      scores <- X %*% self$weights
      probs <- self$softmax(scores)
      return(probs)
    },

    softmax = function(scores) {
      exp_scores <- exp(scores - matrixStats::rowMaxs(scores))
      probs <- exp_scores / matrixStats::rowSums2(exp_scores)
      return(probs)
    },

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

    get_loss_history = function() {
      self$loss_history
    },

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

    # Méthode pour afficher l'importance des variables
    get_variable_importance = function() {
      if (is.null(self$weights)) {
        stop("Le modèle n'est pas encore entraîné.")
      }
      importance <- rowMeans(abs(self$weights))
      importance_df <- data.frame(Variable = rownames(self$weights), Importance = importance)
      importance_df <- importance_df[order(-importance_df$Importance), ]
      return(importance_df)
    },

    # Méthode pour sélectionner les n meilleures variables
    var_select = function(n) {
      importance_df <- self$get_variable_importance()
      top_vars <- importance_df$Variable[1:n]
      return(top_vars)
    },

    accuracy = function(y_test, y_pred) {
      mean(y_test == y_pred)
    },

    confusion_matrix = function(y_test, predictions) {
            
        if (!is.factor(y_test)) {
            y_test <- as.factor(y_test)
        }
        # Afficher la matrice de confusion
        
        predictions <- factor(predictions, levels = levels(y_test))
        confusion_matrix <- confusionMatrix(predictions,y_test)
        
        return(confusion_matrix$table)
    },
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
    print = function() {
        cat("Multinomial Logistic Regression Model\n")
        cat("-------------------------------\n")
        cat("Learning Rate: ", self$learning_rate, "\n")
        cat("Iterations: ", self$iterations, "\n")
        cat("Batch Size: ", self$batch_size, "\n")
    }
  )
)
  
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

