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

Simple.Quali.Imputer <- R6::R6Class("Simple.Quali.Imputer",
                                public = list(
                                  #' @field imputation_type Method that will be used to impute NA.
                                  imputation_type = NULL,
                                  #' @field imputation_values Values produced by the imputer.
                                  imputation_values = NULL,

                                  #' @description
                                  #' This method creates a new qualitative imputer object.
                                  #' @param imputation_type String defining the method of NA imputation.
                                  #' When it equals `mode`, the object is set to perform an imputation based on the mode, i.e. the most frequent modality of the feature.
                                  #' When it equals `probability`, the object is set to perform an imputation based on the probability distribution based on the modality frequencies, i.e. each NA is imputed by one of the possible feature modalities based on a probability weighted by its frequency of appearance.
                                  #' When it equals `remove`, the object is set to perform observations removal if they have missing values
                                  #' @return A new qualitative imputer object.
                                  #' @examples
                                  #' # Creates a new instance of the class
                                  #' imputer <- Simple.Quali.Imputer$new(imputation_type = "probability")
                                  initialize = function(imputation_type = "mode") {
                                    self$imputation_type <- imputation_type
                                  },

                                  #' @description
                                  #' This method fits a qualitative imputer to data given the specified method.
                                  #' @param data A dataframe containing the qualitative data on which the imputer learns to fill missing values.
                                  # #' @param columns A vector listing the names of the columns that will get imputation (must be qualitative features).
                                  #' @return Nothing. The object is internally updated when using this method.
                                  #' @examples
                                  #' # Fits the object to a given dataset
                                  #' imputer$fit(X_train)
                                  fit = function(data) {
                                    if (!is.data.frame(data)){
                                      stop("`data` must be a dataframe. See as.data.frame()")
                                    }
                                    # if ((is.null(columns)) | (!is.vector(columns))){
                                    #   stop("`columns` must be a vector of feature names. See c()")
                                    # }
                                    # if (any(sapply(data[columns], is.numeric))){
                                    #   stop("Features cannot be numeric")
                                    # }
                                    quali <- data[!sapply(data, is.numeric)]
                                    self$imputation_values <- apply(quali, 2, function(x) {
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
                                      } else if (self$imputation_type == "remove") {
                                        x <- x[!is.na(x)]
                                      } else {
                                        stop("Unsupported imputation type")
                                      }
                                    }
                                    )
                                  },

                                  #' @description
                                  #' This method applies on a given dataset the transformation rules the imputer learned from the dataset it was fit to.
                                  #' @param data A dataframe containing the qualitative data that will get missing values imputation.
                                  #' @return A dataframe containing the data with missing values filled.
                                  #' @examples
                                  #' # Performs a qualitative imputation on a given dataset
                                  #' X_test_filled <- imputer$transform(X_test)
                                  transform = function(data) {
                                    if (is.null(self$imputation_values)){
                                      stop("Fit() method must be used before transform()")
                                    }
                                    for (col in names(self$imputation_values)) {
                                      data[[col]][is.na(data[[col]])] <- self$imputation_values[[col]]
                                    }
                                    return(data)
                                  },

                                  #' @description
                                  #' This method fits an imputer on a given dataset then fill the missing values in it.
                                  #' @param data A dataframe containing the qualitative data that will get missing values imputation.
                                  # #' @param columns A vector listing the names of the columns that will get imputation (must be qualitative features).
                                  #' @return A dataframe containing the data with its missing values filled.
                                  #' @examples
                                  #' # Fitting and imputing missing values on a given dataset
                                  #' X_train_filled <- imputer$fit_transform(X_train)
                                  fit_transform = function(data) {
                                    self$fit(data)
                                    return(self$transform(data))
                                  }
                                )
)
