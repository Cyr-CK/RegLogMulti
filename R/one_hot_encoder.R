#' One-Hot Encoder
#' @description
#' Class of a One-Hot Encoder object that has fit, fit_transform and transform methods to use over the qualitative features you want to encode.
#'
#' @import R6
#'
#' @export
#'
#' @examples
#' ohe <- One.Hot.Encoder$new().
#'
#' X_train_ohe <- ohe$fit_transform(X_train).
#'
#' ohe$fit(X_train).
#' X_test_ohe <- ohe$transform(X_test)
One.Hot.Encoder <- R6Class("One.Hot.Encoder",
                               public = list(
                                 #' @field categories (`list()`) \cr
                                 #' Categories of qualitative features.
                                 categories = NULL,

                                 #' @description
                                 #' Create a new one-hot encoder object.
                                 #' @return A new one-hot encoder object.
                                 initialize = function(){
                                   self$categories <- list()
                                 },

                                 #' @description
                                 #' Fit the one-hot encoder to data.
                                 #' @param data (`dataframe`) Dataframe.
                                 #' @return nothing
                                 fit = function(data) {
                                   for (col in names(data)) {
                                     self$categories[[col]] <- unique(data[[col]])
                                   }
                                   invisible(self)
                                 },

                                 #' @description
                                 #' Transform the passed data object based on the fit data object.
                                 #' @param data (`dataframe`) Dataframe.
                                 #' @return Dataframe of one-hot encoded data.
                                 transform = function(data) {
                                   result <- data.frame(row.names = 1:nrow(data))
                                   for (col in names(data)) {
                                     if (col %in% names(self$categories)) {
                                       encoded <- model.matrix(~ 0 + data[[col]])
                                       colnames(encoded) <- paste(col, self$categories[[col]], sep = "_")
                                       result <- cbind(result, encoded)[-1]
                                     } else {
                                       result <- cbind(result, data[[col]])
                                     }
                                   }
                                   result
                                 },


                                 #' @description
                                 #' Fit to data object then one-hot encode it.
                                 #' @param data (`dataframe`) Dataframe.
                                 #' @return Dataframe of one-hot encoded data.
                                 fit_transform = function(data) {
                                   self$fit(data)
                                   self$transform(data)
                                 }

                               )
)
