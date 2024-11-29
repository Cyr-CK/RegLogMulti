#' Robust (Median) Scaler
#'
#' @description
#' Class of a robust (median) scaler object that has fit, fit_transform and transform methods to scale quantitative data.
#'
# #' @details
#'
#' @import R6
#' @export
#'
#' @examples
#' scaler <- Robust.Scaler$new()
#'
#' X_train_scaled <- scaler$fit_transform(X_train)
#'
#' scaler$fit(X_train)
#' X_test_scaled <- scaler$transform(X_test)

Robust.Scaler <- R6::R6Class("Robust.Scaler",
                               public = list(
                                 #' @field median Vector of feature medians.
                                 median = NULL,
                                 #' @field iqr Vector of feature inter-quartile ranges.
                                 iqr = NULL,

                                 #' @description
                                 #' This method creates a new robust scaler object.
                                 #' @return A new standard robust object.
                                 #' @examples
                                 #' # Creates a new instance of the class
                                 #' scaler <- Robust.Scaler$new()
                                 initialize = function(){
                                   self$median <- NULL
                                   self$iqr <- NULL
                                 },

                                 #' @description
                                 #' This method fits a robust scaler on a given dataset.
                                 #' @param data A dataframe containing the quantitative data on which the scaler computes the features' median and inter-quartile range.
                                 #' @return Nothing. The object is internally updated when using this method.
                                 #' @examples
                                 #' # Fits the object to a given dataset
                                 #' scaler$fit(X_train)
                                 fit = function(data) {
                                   quanti <- data[sapply(data,is.numeric)]
                                   self$median <- sapply(quanti, median, na.rm = TRUE)
                                   self$iqr <- apply(quanti, 2, function(x){
                                     x <- x[!is.na(x)]
                                     quantile(x, 0.75) - quantile(x, 0.25)
                                   })
                                   invisible(self)
                                 },

                                 #' @description
                                 #' This method applies on a given dataset the transformation rules the scaler learned from the dataset it was fit to.
                                 #' @param data A dataframe containing the quantitative data that will get robust scaling.
                                 #' @return A dataframe containing the data after robust scaling.
                                 #' @examples
                                 #' # Performs a robust scaling on a given dataset
                                 #' X_test_scaled <- scaler$transform(X_test)
                                 transform = function(data) {
                                   if (is.null(self$median) || is.null(self$iqr)) {
                                     stop("Fit the scaler before transforming data")
                                   }
                                   quanti <- data[sapply(data, is.numeric)]
                                   data[names(quanti)] <- t((t(quanti) - self$median) / self$iqr)
                                   data
                                 },

                                 #' @description
                                 #' This method fits a robust scaler on a given dataset then applies robust scaling on it.
                                 #' @param data A dataframe containing the quantitative data that will get robust scaling.
                                 #' @return A dataframe containing the data after robust scaling.
                                 #' @examples
                                 #' # Fitting and robust scaling on a given dataset
                                 #' X_train_scaled <- scaler$fit_transform(X_train)
                                 fit_transform = function(data) {
                                   self$fit(data)
                                   self$transform(data)
                                 }
                               )
)
