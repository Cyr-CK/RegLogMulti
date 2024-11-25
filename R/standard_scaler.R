#' Standardization Scaler
#'
#' @description
#' Class of a Standardization scaler object that has fit, fit_transform and transform methods to standardize quantitative data
#'
#' @details
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
#'
Standard.Scaler <- R6::R6Class("Standard.Scaler",
                           public = list(
                             #' @field mean Vector of feature means
                             mean = NULL,
                             #' @field std Vector of feature standard-deviations
                             std = NULL,

                             initialize = function(){
                               self$mean <- NULL
                               self$std <- NULL
                             },

                             fit = function(data) {
                               self$mean <- apply(data,2, mean, na.rm = TRUE)
                               self$std <- apply(data, 2, sd, na.rm = TRUE)
                               invisible(self)
                             },

                             transform = function(data) {
                               if (is.null(self$mean) || is.null(self$std)) {
                                 stop("Fit the scaler before transforming data")
                               }
                               t((t(data) - self$mean) / self$std)
                             },

                             fit_transform = function(data) {
                               self$fit(data)
                               self$transform(data)
                             }
                           )
)
