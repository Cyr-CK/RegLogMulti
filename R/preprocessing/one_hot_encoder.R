#' One-Hot Encoder (OHE)
#'
#' @description
#' Class of a One-Hot Encoder (OHE) object that has fit, fit_transform and transform methods to use over the qualitative features you want to encode.
#'
# #' @details#'
#'
#' @import R6
#' @export
#'
#' @examples
#' ohe <- One.Hot.Encoder$new().
#'
#' X_train_ohe <- ohe$fit_transform(X_train).
#'
#' ohe$fit(X_train).
#' X_test_ohe <- ohe$transform(X_test)

One.Hot.Encoder <- R6::R6Class("One.Hot.Encoder",
                               public = list(
                                 #' @field categories Categories of qualitative features.
                                 categories = NULL,
                                 #' @field drop.last Whether last modality of qualitative feature is removed or not.
                                 drop.last = NULL,

                                 #' @description
                                 #' This method initializes a one-hot encoder (OHE) object by setting up the necessary internal parameters.
                                 #' It is automatically called when a new object of this class is created.
                                 #' @return A new OHE model object.
                                 #' @examples
                                 #' # Creates a new instance of the class
                                 #' ohe <- One.Hot.Encoder$new()
                                 initialize = function(drop.last=TRUE){
                                   self$categories <- list()
                                   self$drop.last <- drop.last
                                 },

                                 #' @description
                                 #' This method fits an OHE on a given dataset.
                                 #' @param data A dataframe containing the qualitative data from which the object learns to perform future one-hot encoding.
                                 #' @return Nothing. The object is internally updated when using this method.
                                 #' @examples
                                 #' # Fits the object to a given dataset
                                 #' ohe$fit(X_train)
                                 fit = function(data) {
                                   if (!is.data.frame(data)){
                                     stop("`data` must be a dataframe. See as.data.frame()")
                                   }
                                   quali <- data[!sapply(data, is.numeric)]
                                   for (col in names(quali)) {
                                     self$categories[[col]] <- unique(quali[[col]])
                                   }
                                   invisible(self)
                                 },

                                 #' @description
                                 #' This method applies on a given dataset the transformation rules the OHE learned from the dataset it was fit to.
                                 #' @details
                                 #' If `drop.last=TRUE`, then after transformation, for each features (which can only be qualitative), the one-hot encoded column of the last modality is automatically dropped in order to avoid multicolinearity issue when modelling.
                                 #' @param data A dataframe containing the qualitative data that will undergo one-hot encoding.
                                 #' @return A (sparse) dataframe containing the one-hot encoded qualitative features.
                                 #' @examples
                                 #' # One-hot encoding a given dataset
                                 #' X_test_ohe <- ohe$transform(X_test)
                                 transform = function(data) {
                                   if (is.null(self$categories)){
                                     stop("Fit() method must be used before transform()")
                                   }
                                   result <- data.frame(row.names = 1:nrow(data))
                                   for (col in names(data)) {
                                     if (col %in% names(self$categories)) {
                                       encoded <- model.matrix(~ 0 + data[[col]])
                                       colnames(encoded) <- paste(col, self$categories[[col]], sep = "_")
                                       if (self$drop.last){
                                         result <- cbind(result, encoded)[-1]
                                       } else{
                                         result <- cbind(result, encoded)
                                       }
                                     } else {
                                       result <- cbind(result, data[[col]])
                                     }
                                   }
                                   quanti <- data[sapply(data, is.numeric)]
                                   result <- cbind(quanti, result)
                                   result
                                 },

                                 #' @description
                                 #' This method fits an OHE on a given dataset then transforms it.
                                 #' @details
                                 #' After transformation, for each features (which can only be qualitative), the one-hot encoded column corresponding to the last modality is automatically dropped in order to avoid multicolinearity issue when modelling.
                                 #' @param data A dataframe containing the qualitative data to which the object will be fit and perform one-hot encoding.
                                 #' @return A (sparse) dataframe containing the one-hot encoded qualitative features.
                                 #' @examples
                                 #' # Fitting and one-hot encoding a given dataset
                                 #' X_train_ohe <- ohe$fit_transform(X_train)
                                 fit_transform = function(data) {
                                   self$fit(data)
                                   return(self$transform(data))
                                 }

                               )
)

