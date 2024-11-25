Robust.Scaler <- R6::R6Class("Robust.Scaler",
                               public = list(
                                 #' @field mean Vector of feature means
                                 median = NULL,
                                 #' @field std Vector of feature standard-deviations
                                 iqr = NULL,

                                 initialize = function(){
                                   self$median <- NULL
                                   self$iqr <- NULL
                                 },

                                 fit = function(data) {
                                   self$median <- apply(data,2, median, na.rm = TRUE)
                                   self$iqr <- apply(data, 2,
                                                     function(x) quantile(x, 0.75) - quantile(x, 0.25),
                                                     na.rm = TRUE)
                                   invisible(self)
                                 },

                                 transform = function(data) {
                                   if (is.null(self$median) || is.null(self$iqr)) {
                                     stop("Fit the scaler before transforming data")
                                   }
                                   t((t(data) - self$median) / self$iqr)
                                 },

                                 fit_transform = function(data) {
                                   self$fit(data)
                                   self$transform(data)
                                 }
                               )
)
