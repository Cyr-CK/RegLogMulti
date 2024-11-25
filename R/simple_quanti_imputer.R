#' Simple quantitative NA imputer
#'
#' @description
#' Class of a quantitative NA imputer object that has fit, fit_transform and transform methods to impute quantitative missing values with mean or median method
#'
#' @details
#'
#' @import R6
#' @export
#'
#' @examples
#' imputer <- Simple.Quanti.Imputer$new(imputation_type="mean")
#' imputer <- Simple.Quanti.Imputer$new(imputation_type="median")
#'
#' X_train_filled <- imputer$fit_transform(X_train, c("Col_quanti_1","Col_quanti_2","Col_quanti_N"))
#'
#' imputer$fit(X_train, c("Col_quanti_1","Col_quanti_2","Col_quanti_N"))
#' X_test_filled <- imputer$transform(X_test)

Simple.Quanti.Imputer <- R6::R6Class("Simple.Quanti.Imputer",
                                 public = list(
                                   #' @field imputation_type Method that will be used to impute NA
                                   imputation_type = NULL,
                                   #' @field imputation_values Values produced by the imputer
                                   imputation_values = NULL,

                                   #' @description
                                   #' Create a new simple quantitative imputer object
                                   #' @param imputation_type Method of NA imputation
                                   #' @return A new imputer object
                                   initialize = function(imputation_type = "mean") {
                                     self$imputation_type <- imputation_type
                                   },

                                   #' @description
                                   #' Fit the simple quantitative imputer to data following the specified method
                                   #' @param data Dataframe
                                   #' @param columns Vector of column names that will get imputation (must be of quantitative type)
                                   fit = function(data, columns) {
                                     self$imputation_values <- apply(data[columns], 2, function(x) {
                                       if (self$imputation_type == "mean") {
                                         mean(x, na.rm = TRUE)
                                       } else if (self$imputation_type == "median") {
                                         median(x, na.rm = TRUE)
                                       } else {
                                         stop("Unsupported imputation type")
                                       }
                                     }
                                     )
                                   },

                                   #' @description
                                   #' Transform the passed data object based on the fit data object
                                   #' @param data Dataframe
                                   #' @return Dataframe of data after imputation
                                   transform = function(data) {
                                     for (col in names(self$imputation_values)) {
                                       data[[col]][is.na(data[[col]])] <- self$imputation_values[[col]]
                                     }
                                     return(data)
                                   },

                                   #' @description
                                   #' Fit to data object following the specified method then impute NA
                                   #' @param data Dataframe
                                   #' @param columns Vector of column names that will get imputation (must be of quantitative type)
                                   #' @return Dataframe of data after imputation
                                   fit_transform = function(data, columns) {
                                     self$fit(data, columns)
                                     return(self$transform(data))
                                   }
                                 )
)
