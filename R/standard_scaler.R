#' Standard Scaler
#'
#' @description
#' Class of a standardization scaler object that has fit, fit_transform and transform methods to standardize quantitative data.
#'
# #' @details
#'
#' @import R6
#' @export
#'
#' @examples
#' scaler <- Standard.Scaler$new()
#'
#' X_train_scaled <- scaler$fit_transform(X_train)
#'
#' scaler$fit(X_train)
#' X_test_scaled <- scaler$transform(X_test)

Standard.Scaler <- R6::R6Class("Standard.Scaler",
                           public = list(
                             #' @field mean Vector of feature means.
                             mean = NULL,
                             #' @field std Vector of feature standard-deviations.
                             std = NULL,


                             #' @description
                             #' This method creates a new standard scaler object.
                             #' @return A new standard scaler object.
                             #' @examples
                             #' # Creates a new instance of the class
                             #' scaler <- Standard.Scaler$new()
                             initialize = function(){
                               self$mean <- NULL
                               self$std <- NULL
                             },

                             #' @description
                             #' This method fits a standard scaler on a given dataset.
                             #' @param data A dataframe containing the quantitative data on which the scaler computes the features' mean and standard deviation.
                             #' @return Nothing. The object is internally updated when using this method.
                             #' @examples
                             #' # Fits the object to a given dataset
                             #' scaler$fit(X_train)
                             fit = function(data) {
                               if (!is.data.frame(data)){
                                 stop("`data` must be a dataframe. See as.data.frame()")
                               }
                               quanti <- data[sapply(data,is.numeric)]
                               if (ncol(quanti) == 0){
                                 stop("No quantitative features in your dataset")
                               }
                               self$mean <- apply(quanti,2, mean, na.rm = TRUE)
                               self$std <- apply(quanti, 2, sd, na.rm = TRUE)
                               invisible(self)
                             },

                             #' @description
                             #' This method applies on a given dataset the transformation rules the scaler learned from the dataset it was fit to.
                             #' @param data A dataframe containing the quantitative data that will get standardization.
                             #' @return A dataframe containing the data after standardization.
                             #' @examples
                             #' # Performs a standardization scaling on a given dataset
                             #' X_test_scaled <- scaler$transform(X_test)
                             transform = function(data) {
                               if (is.null(self$mean) || is.null(self$std)) {
                                 stop("Fit() method must be used before transform()")
                               }
                               if (!is.data.frame(data)){
                                 stop("`data` must be a dataframe. See as.data.frame()")
                               }
                               quanti <- data[sapply(data, is.numeric)]
                               if (ncol(quanti) == 0){
                                 stop("No quantitative features in your dataset")
                               }
                               data[names(quanti)] <- t((t(quanti) - self$mean) / self$std)
                               data
                             },

                             #' @description
                             #' This method fits a standard scaler on a given dataset then applies standardization on it.
                             #' @param data A dataframe containing the quantitative data that will get standardization.
                             #' @return A dataframe containing the data after standardization.
                             #' @examples
                             #' # Fitting and standardization scaling on a given dataset
                             #' X_train_scaled <- scaler$fit_transform(X_train)
                             fit_transform = function(data) {
                               self$fit(data)
                               self$transform(data)
                             }
                           )
)
