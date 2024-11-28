#' Simple quantitative NA imputer
#'
#' @description
#' Class of a quantitative NA imputer object that has fit, fit_transform and transform methods to impute quantitative missing values with mean or median method.
#'
# #' @details
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
                                   #' @field imputation_type Method that will be used to impute NA.
                                   imputation_type = NULL,
                                   #' @field imputation_values Values produced by the imputer.
                                   imputation_values = NULL,

                                   #' @description
                                   #' This method creates a new quantitative imputer object.
                                   #' @param imputation_type String defining the method of NA imputation.
                                   #' When it equals `mean`, the object is set to perform an imputation based on the mean, i.e. the average value of the feature.
                                   #' When it equals `median`, the object is set to perform an imputation based on the median, i.e. the median value of the feature.
                                   #' @return A new quantitative imputer object.
                                   #' @examples
                                   #' # Creates a new instance of the class
                                   #' imputer <- Simple.Quanti.Imputer$new(imputation_type = "median")
                                   initialize = function(imputation_type = "mean") {
                                     self$imputation_type <- imputation_type
                                   },

                                   #' @description
                                   #' This method fits a quantitative imputer to data given the specified method.
                                   #' @param data A dataframe containing the quantitative data on which the imputer learns to fill missing values.
                                   #' @param columns A vector listing the names of the columns that will get imputation (must be quantitative features).
                                   #' @return Nothing. The object is internally updated when using this method.
                                   #' @examples
                                   #' # Fits the object to a given dataset
                                   #' imputer$fit(X_train)
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
                                   #' This method applies on a given dataset the transformation rules the imputer learned from the dataset it was fit to.
                                   #' @param data A dataframe containing the quantitative data that will get missing values imputation.
                                   #' @return A dataframe containing the data with its missing values filled.
                                   #' @examples
                                   #' # Performs a quantitative imputation on a given dataset
                                   #' X_test_filled <- imputer$transform(X_test)
                                   transform = function(data) {
                                     for (col in names(self$imputation_values)) {
                                       data[[col]][is.na(data[[col]])] <- self$imputation_values[[col]]
                                     }
                                     return(data)
                                   },

                                   #' @description
                                   #' This method fits an imputer on a given dataset then fill the missing values in it.
                                   #' @param data A dataframe containing the quantitative data that will get missing values imputation.
                                   #' @param columns A vector listing the names of the columns that will get imputation (must be quantitative features).
                                   #' @return A dataframe containing the data with its missing values filled.
                                   #' @examples
                                   #' # Fitting and imputing missing values on a given dataset
                                   #' X_train_filled <- imputer$fit_transform(X_train)
                                   fit_transform = function(data, columns) {
                                     self$fit(data, columns)
                                     return(self$transform(data))
                                   }
                                 )
)
