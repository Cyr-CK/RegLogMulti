#' Min-Max Scaler
#'
#' @description
#' Class of a Min-Max scaler object that has fit, fit_transform and transform methods to min-max scale quantitative data.
#'
# #' @details
#'
#' @import R6
#' @export
#'
#' @examples
#' scaler <- MinMax.Scaler$new()
#'
#' X_train_scaled <- scaler$fit_transform(X_train)
#'
#' scaler$fit(X_train)
#' X_test_scaled <- scaler$transform(X_test)

MinMax.Scaler <- R6::R6Class("MinMax.Scaler",
                               public = list(
                                 #' @field min Vector of feature minimums.
                                 min = NULL,
                                 #' @field max Vector of feature maximums.
                                 max = NULL,

                                 #' @description
                                 #' This method creates a new min-max scaler object.
                                 #' @return A new min-max scaler object.
                                 #' @examples
                                 #' # Creates a new instance of the class
                                 #' scaler <- MinMax.Scaler$new()
                                 initialize = function(){
                                   self$min <- NULL
                                   self$max <- NULL
                                 },

                                 #' @description
                                 #' This method fits a min-max scaler on a given dataset.
                                 #' @param data A dataframe containing the quantitative data on which the scaler computes the features' minimum and maximum.
                                 #' @return Nothing. The object is internally updated when using this method.
                                 #' @examples
                                 #' # Fits the object to a given dataset
                                 #' scaler$fit(X_train)
                                 fit = function(data) {
                                   self$min <- apply(data,2, min, na.rm = TRUE)
                                   self$max <- apply(data, 2, max, na.rm = TRUE)
                                   invisible(self)
                                 },

                                 #' @description
                                 #' This method applies on a given dataset the transformation rules the scaler learned from the dataset it was fit to.
                                 #' @param data A dataframe containing the quantitative data that will get min-max scaling.
                                 #' @return A dataframe containing the data after min-max scaling.
                                 #' @examples
                                 #' # Performs a min-max scaling on a given dataset
                                 #' X_test_scaled <- scaler$transform(X_test)
                                 transform = function(data) {
                                   if (is.null(self$min) || is.null(self$max)) {
                                     stop("Fit the scaler before transforming data")
                                   }
                                   t((t(data) - self$min) / (self$max - self$min))
                                 },

                                 #' @description
                                 #' This method fits a min-max scaler on a given dataset then applies min-max scaling on it.
                                 #' @param data A dataframe containing the quantitative data that will get min-max scaling.
                                 #' @return A dataframe containing the data after min-max scaling.
                                 #' @examples
                                 #' # Fitting and min-max scaling on a given dataset
                                 #' X_train_scaled <- scaler$fit_transform(X_train)
                                 fit_transform = function(data) {
                                   self$fit(data)
                                   self$transform(data)
                                 }
                               )
)
