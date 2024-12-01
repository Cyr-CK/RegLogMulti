#' Simple qualitative NA imputer
#'
#' @description
#' Class of a qualitative NA imputer object that has fit, fit_transform and transform methods to impute qualitative missing values with remove, mode or probabilistic method.
#'
# #' @details
#'
#' @import R6
#' @export
#'
#' @examples
#' imputer <- Simple.Quali.Imputer$new(imputation_type="mode")
#' imputer <- Simple.Quali.Imputer$new(imputation_type="probability")
#'
#' X_train_filled <- imputer$fit_transform(X_train, c("Col_quali_1","Col_quali_2","Col_quali_N"))
#'
#' imputer$fit(X_train, c("Col_quali_1","Col_quali_2","Col_quali_N"))
#' X_test_filled <- imputer$transform(X_test)

library(R6)

Simple.Quali.Imputer <- R6::R6Class("Simple.Quali.Imputer",
  public = list(
    imputation_values = NULL,
    
    initialize = function() {
      self$imputation_values <- list()
    },
    
    # Ajuster l'imputer en calculant la modalité la plus fréquente pour chaque colonne
    fit = function(data, cols) {
      for (col in cols) {
        # Calculer la modalit�� la plus fréquente
        mode_val <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
        self$imputation_values[[col]] <- mode_val
      }
    },
    
    # Transformer les données en imputant les valeurs manquantes
    transform = function(data, cols) {
      for (col in cols) {
        data[is.na(data[[col]]), col] <- self$imputation_values[[col]]
      }
      return(data)
    },
    
    # Combiner fit et transform
    fit_transform = function(data, cols) {
      self$fit(data, cols)
      data <- self$transform(data, cols)
      return(data)
    }
  )
)
