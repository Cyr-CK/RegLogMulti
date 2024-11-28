library(R6)
if (!require(matrixStats)) {
    install.packages("matrixStats")
    library(matrixStats)
}

library(matrixStats) # Charger le package matrixStats pour des opérations optimisées

MultinomialLogisticRegression <- R6Class("MultinomialLogisticRegression",
    public = list(
        weights = NULL,
        learning_rate = NULL,
        initial_learning_rate = NULL, # Store the initial learning rate
        iterations = NULL,
        batch_size = NULL,
        regularization = NULL,
        regularization_strength = NULL,
        classes = NULL,
        class_frequencies = NULL,
        selected_features = NULL,
        loss_function = NULL,
        importance_features = NULL,
        loss_history = NULL,
        optimizer = NULL,
        beta1 = NULL,
        beta2 = NULL,
        epsilon = NULL,
        m = NULL,
        v = NULL,
        t = NULL,
        initialization = NULL,
        lr_decay = NULL,           # Added learning rate decay flag
        decay_rate = NULL,         # Added decay rate parameter

        initialize = function(learning_rate = 0.01, iterations = 100, 
                              batch_size = NULL, 
                              regularization = NULL, 
                              regularization_strength = 0.01, 
                              loss_function = "cross_entropy",
                              optimizer = "gradient_descent",
                              beta1 = 0.9,
                              beta2 = 0.999,
                              epsilon = 1e-8,
                              initialization = "xavier",
                              lr_decay = FALSE,        # Default to no decay
                              decay_rate = 0.001) {    # Default decay rate
            self$learning_rate <- learning_rate
            self$initial_learning_rate <- learning_rate
            self$iterations <- iterations
            self$batch_size <- batch_size
            self$regularization <- regularization
            self$regularization_strength <- regularization_strength
            self$selected_features <- NULL
            self$loss_history <- numeric(0)
            self$optimizer <- optimizer
            self$beta1 <- beta1
            self$beta2 <- beta2
            self$epsilon <- epsilon
            self$m <- NULL
            self$v <- NULL
            self$t <- 0
            self$initialization <- initialization
            self$lr_decay <- lr_decay
            self$decay_rate <- decay_rate
            if (!(loss_function %in% c("cross_entropy", "squared_error"))) {
                stop("Unsupported loss function. Choose 'cross_entropy' or 'squared_error'.")
            }
            self$loss_function <- loss_function
            if (!(initialization %in% c("zeros", "xavier"))) {
                stop("Unsupported initialization. Choose 'zeros' or 'xavier'.")
            }
            if (!is.logical(self$lr_decay)) {
                stop("lr_decay must be a logical value (TRUE or FALSE).")
            }
            if (!is.numeric(self$decay_rate) || self$decay_rate < 0) {
                stop("decay_rate must be a non-negative numeric value.")
            }
        },

        softmax = function(z) {
            # Utiliser matrixStats pour optimiser les performances sur de grands ensembles de données
            z_max <- rowMaxs(z) # Fonction optimisée pour obtenir le max par ligne
            exp_z <- exp(z - z_max)
            sum_exp_z <- rowSums2(exp_z) # Fonction optimisée pour la somme par ligne
            return(exp_z / sum_exp_z)
        },

        fit = function(X, y) {
            if(!is.factor(y)) {
                y <- as.factor(y)
            }
            self$classes <- levels(y)
            self$class_frequencies <- table(y) / length(y)

            if (!is.null(self$selected_features)) {
                X <- X[, self$selected_features, drop = FALSE]
            }

            n_samples <- nrow(X)
            n_features <- ncol(X)
            n_classes <- length(self$classes)

            # Initialize weights
            if (self$initialization == "zeros") {
                self$weights <- matrix(0, nrow = n_features, ncol = n_classes)
            } else if (self$initialization == "xavier") {
                limit <- sqrt(6 / (n_features + n_classes))
                self$weights <- matrix(runif(n_features * n_classes, min = -limit, max = limit), 
                                       nrow = n_features, ncol = n_classes)
            }

            y_one_hot <- matrix(0, nrow = n_samples, ncol = n_classes)
            for (i in 1:n_samples) {
                class_index <- which(self$classes == y[i])
                y_one_hot[i, class_index] <- 1
            }

            self$loss_history <- numeric(0)

            # Initialize optimizer-specific parameters
            if (self$optimizer == "adam") {
                self$m <- matrix(0, nrow = n_features, ncol = n_classes)
                self$v <- matrix(0, nrow = n_features, ncol = n_classes)
                self$t <- 0
            }

            # Initialize progress bar
            pb <- txtProgressBar(min = 0, max = self$iterations, style = 3)

            for (i in 1:self$iterations) {
                if (is.null(self$batch_size)) {
                    indices <- 1:n_samples
                } else {
                    indices <- sample(1:n_samples, self$batch_size, replace = TRUE)
                }
                X_batch <- X[indices, , drop = FALSE]
                y_batch <- y_one_hot[indices, , drop = FALSE]

                scores <- X_batch %*% self$weights
                probs <- self$softmax(scores)

                if (self$loss_function == "cross_entropy") {
                    epsilon_val <- 1e-15
                    probs_clipped <- pmax(pmin(probs, 1 - epsilon_val), epsilon_val)
                    loss <- -mean(rowSums(y_batch * log(probs_clipped)))
                    error <- probs - y_batch
                } else if (self$loss_function == "squared_error") {
                    loss <- mean((probs - y_batch)^2)
                    error <- (probs - y_batch) * probs * (1 - probs)
                }

                self$loss_history <- c(self$loss_history, loss)

                gradient <- t(X_batch) %*% error / nrow(X_batch)

                if (!is.null(self$regularization)) {
                    if (self$regularization == "L2") {
                        gradient <- gradient + self$regularization_strength * self$weights
                    } else if (self$regularization == "L1") {
                        gradient <- gradient + self$regularization_strength * sign(self$weights)
                    }
                }

                # Apply learning rate decay if enabled
                if (self$lr_decay) {
                    self$learning_rate <- self$initial_learning_rate / (1 + self$decay_rate * i)
                }

                if (self$optimizer == "gradient_descent") {
                    self$weights <- self$weights - self$learning_rate * gradient
                } else if (self$optimizer == "adam") {
                    self$t <- self$t + 1
                    self$m <- self$beta1 * self$m + (1 - self$beta1) * gradient
                    self$v <- self$beta2 * self$v + (1 - self$beta2) * (gradient^2)
                    m_hat <- self$m / (1 - self$beta1^self$t)
                    v_hat <- self$v / (1 - self$beta2^self$t)
                    self$weights <- self$weights - self$learning_rate * m_hat / (sqrt(v_hat) + self$epsilon)
                }

                setTxtProgressBar(pb, i)
            }

            close(pb)
        },

        predict = function(X) {
            if (!is.null(self$selected_features)) {
                X <- X[, self$selected_features, drop = FALSE]
            }
            scores <- X %*% self$weights
            probs <- self$softmax(scores)
            pred_indices <- max.col(probs)
            return(self$classes[pred_indices])
        },

        predict_proba = function(X) {
            if (!is.null(self$selected_features)) {
                X <- X[, self$selected_features, drop = FALSE]
            }
            scores <- X %*% self$weights
            probs <- self$softmax(scores)
            colnames(probs) <- self$classes
            return(probs)
        },

        print = function() {
            cat("Multinomial Logistic Regression Model\n")
            cat("Classes:", paste(self$classes, collapse = ", "), "\n")
            cat("Number of iterations:", self$iterations, "\n")
            cat("Initial Learning rate:", self$initial_learning_rate, "\n")
            cat("Current Learning rate:", self$learning_rate, "\n")
            cat("Optimizer:", self$optimizer, "\n")
            if (self$optimizer == "adam") {
                cat("Beta1:", self$beta1, "\n")
                cat("Beta2:", self$beta2, "\n")
                cat("Epsilon:", self$epsilon, "\n")
            }
            cat("Loss function:", self$loss_function, "\n")
            if (!is.null(self$regularization)) {
                cat("Regularization:", self$regularization, "\n")
                cat("Regularization Strength:", self$regularization_strength, "\n")
            }
            cat("Initialization Method:", self$initialization, "\n")
            cat("Learning Rate Decay:", self$lr_decay, "\n")
            if (self$lr_decay) {
                cat("Decay Rate:", self$decay_rate, "\n")
            }
            if (!is.null(self$selected_features)) {
                cat("Selected Features:", paste(self$selected_features, collapse = ", "), "\n")
            }
            if (!is.null(self$batch_size)) {
                cat("Batch Size:", self$batch_size, "\n")
            }
        },

        summary = function() {
            self$print()
            cat("Class Frequencies:\n")
            print(self$class_frequencies)
            cat("Weights:\n")
            print(self$weights)
            cat("Loss History:\n")
            print(self$loss_history)
        },

        var_importance = function() {
            if (is.null(self$weights)) {
                stop("Model has not been trained yet.")
            }
            importance <- rowMeans(abs(self$weights)) # Utiliser rowMeans au lieu de apply pour la performance
            importance_df <- data.frame(Feature = rownames(self$weights), Importance = importance)
            importance_df <- importance_df[order(-importance_df$Importance), ]
            return(importance_df)
        },

        var_select = function(n = 5) {
            importance_df <- self$var_importance()
            selected <- head(importance_df$Feature, n)
            self$selected_features <- selected
            cat("Selected Features:", paste(self$selected_features, collapse = ", "), "\n")
        },

        plot_loss_history = function() {
            if(length(self$loss_history) == 0) {
                cat("No loss history to plot.\n")
                return(NULL)
            }
            plot(self$loss_history, type = "l", 
                 main = "Loss History", 
                 xlab = "Iteration", 
                 ylab = "Loss",
                 col = "blue",
                 lwd = 2)
        }
    )
)
# Generate synthetic data

n_samples <- 1000000
n_features <- 50
n_classes <- 10

print("Generating synthetic data...")
X <- matrix(rnorm(n_samples * n_features), nrow = n_samples, ncol = n_features)
colnames(X) <- paste0("Feature", 1:n_features)

# Poids vrais pour générer les étiquettes
true_weights <- matrix(rnorm(n_features * n_classes), nrow = n_features, ncol = n_classes)
scores <- X %*% true_weights
probs <- rowLogSumExps(scores) # Utilisation de matrixStats pour une somme log optimisée
probs <- exp(scores - probs)   # Calcul de la probabilité softmax

# Attribution des classes basées sur les probabilités
y <- max.col(probs)
y <- factor(y, levels = 1:n_classes)

print("Fitting model...")
# Initialize and train the model
model <- MultinomialLogisticRegression$new(
    learning_rate = 0.05, 
    iterations = 1000, 
    regularization = "L2", 
    regularization_strength = 0.01, 
    batch_size = 64, 
    loss_function = "cross_entropy",
    # optimizer = "adam",        # Use Adam optimizer
    beta1 = 0.9,                
    beta2 = 0.999,              
    epsilon = 1e-8,
    # initialization = "xavier", # Added initialization parameter
    lr_decay = TRUE,           # Enable learning rate decay
    decay_rate = 0.001         # Set decay rate
)

model$fit(X, y)

# Make predictions
predictions <- model$predict(X)

# Calculate accuracy
accuracy <- mean(predictions == y)
cat("Accuracy:", accuracy, "\n")

# Display model summary
# model$print()
# model$summary()
model$plot_loss_history()