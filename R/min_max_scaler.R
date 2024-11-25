MinMax.Scaler <- R6::R6Class("MinMax.Scaler",
                               public = list(
                                 #' @field min Vector of feature minimums
                                 min = NULL,
                                 #' @field max Vector of feature maximums
                                 max = NULL,

                                 initialize = function(){
                                   self$min <- NULL
                                   self$max <- NULL
                                 },

                                 fit = function(data) {
                                   self$min <- apply(data,2, min, na.rm = TRUE)
                                   self$max <- apply(data, 2, max, na.rm = TRUE)
                                   invisible(self)
                                 },

                                 transform = function(data) {
                                   if (is.null(self$min) || is.null(self$max)) {
                                     stop("Fit the scaler before transforming data")
                                   }
                                   t((t(data) - self$min) / (self$max - self$min))
                                 },

                                 fit_transform = function(data) {
                                   self$fit(data)
                                   self$transform(data)
                                 }
                               )
)
