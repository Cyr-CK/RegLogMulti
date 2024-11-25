#' Simple qualitative NA imputer
#'
#' @description
#' Class of a qualitative NA imputer object that has fit, fit_transform and transform methods to impute qualitative missing values with mode or probabilistic method
#'
#' @details
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

Simple.Quali.Imputer <- R6::R6Class("Simple.Quali.Imputer",
                                public = list(
                                  #' @field imputation_type Method that will be used to impute NA
                                  imputation_type = NULL,
                                  #' @field imputation_values Values produced by the imputer
                                  imputation_values = NULL,

                                  #' @description
                                  #' Create a new simple qualitative imputer object
                                  #' @param imputation_type Method of NA imputation
                                  #' @return A new imputer object
                                  initialize = function(imputation_type = "mode") {
                                    self$imputation_type <- imputation_type
                                  },

                                  #' @description
                                  #' Fit the simple qualitative imputer to data following the specified method
                                  #' @param data Dataframe
                                  #' @param columns Vector of column names that will get imputation (must be of qualitative type)
                                  fit = function(data, columns) {
                                    self$imputation_values <- apply(data[columns], 2, function(x) {
                                      if (self$imputation_type == "mode") {
                                        Mode <- function(x, na.rm = FALSE) {
                                          if (na.rm) {
                                            x <- x[!is.na(x)]
                                          }
                                          unique_values <- unique(x)
                                          frequencies <- tabulate(match(x, unique_values))
                                          mode_values <- unique_values[frequencies == max(frequencies)]
                                          return(mode_values)
                                        }

                                        Mode(x, na.rm = TRUE)
                                      } else if (self$imputation_type == "probability") {
                                        # Remove NA values for probability calculation
                                        non_na_values <- x[!is.na(x)]

                                        # Calculate probability distribution
                                        prob_dist <- table(non_na_values) / length(non_na_values)

                                        # Impute missing values
                                        sample(
                                          names(prob_dist),
                                          sum(is.na(x)),
                                          replace = TRUE,
                                          prob = prob_dist
                                        )
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
                                  #' @param columns Vector of column names that will get imputation (must be of qualitative type)
                                  #' @return Dataframe of data after imputation
                                  fit_transform = function(data, columns) {
                                    self$fit(data, columns)
                                    return(self$transform(data))
                                  }
                                )
)
