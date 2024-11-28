#' Factor Analysis of Mixed Data (FAMD)
#'
#' @description
#' Class of a Factorial Analysis of Mixed Data (FAMD) object that has fit, fit_transform and transform methods to embed the qualitative and quantitative features into a new quantitative coordinates system.
#'
# #' @details
#'
#' @import R6
#' @import FactoMineR
#' @export
#'
#' @examples
#' famd <- FAMD_$new(n_components = 2)
#'
#' X_train_famd <- famd$fit_transform(X_train)
#'
#' famd$fit(X_train)
#' X_test_famd <- famd$transform(X_test)

FAMD_ <- R6::R6Class("FAMD_",
                 public = list(
                   #' @field model The FAMD object.
                   model = NULL,
                   #' @field n_components Number of components to keep from the FAMD factorial space.
                   n_components = NULL,

                   #' @description
                   #' This method initializes a FAMD model by setting up the necessary internal parameters.
                   #' It is automatically called when a new object of this class is created.
                   #' @param n_components Number of components to keep from the FAMD factorial space.
                   #' When no value is specified, it is automatically set to `ncol(data)`.
                   #' @return A new FAMD model object.
                   #' @examples
                   #' # Creates a new instance of the class
                   #' famd <- FAMD_$new(n_components = 2)
                   initialize = function(n_components = NULL){
                     self$n_components <- n_components
                   },

                   #' @description
                   #' This method fits a FAMD model on a given dataset.
                   #' @param data A dataframe containing the qualitative data from which the object learns FAMD data representation.
                   #' @return Nothing. The object is internally updated when using this method.
                   #' @examples
                   #' # Fits the object to a given dataset
                   #' famd$fit(X_train)
                   fit = function(data){
                     if (!is.data.frame(data)){
                       stop("Data are not in a dataframe format")
                     }
                     if (is.null(self$n_components)){
                       self$n_components <- ncol(data)
                     }
                     self$model <- FactoMineR::FAMD(data, ncp=self$n_components, graph=FALSE)
                     invisible(self)
                   },

                   #' @description
                   #' This method applies on a given dataset the transformation rules the FAMD model learned from the dataset it was fit to.
                   #' @param data A dataframe containing the quantitative and qualitative data that will undergo FAMD modelisation.
                   #' @return A dataframe containing the coordinates of the new data representation in the FAMD factorial space.
                   #' @examples
                   #'# Performs a FAMD on a given dataset
                   #' X_test_famd <- famd$transform(X_test)
                   transform = function(data){
                     if (is.null(self$model)){
                       stop("Fit() method must be used before transform()")
                     }
                     coord <- predict(self$model, data)$coord

                     return(as.data.frame(coord[, 1:self$n_components]))
                   },

                   #' @description
                   #' This method fits a FAMD model on a given dataset then transforms it.
                   #' @param data A dataframe containing the quantitative and qualitative data to which the object will be fit and will perform FAMD.
                   #' @return A dataframe containing the coordinates of the new data representation in the FAMD factorial space.
                   #' @examples
                   #' # Fitting and FAMD modelling a given dataset
                   #' X_train_famd <- famd$fit_transform(X_train)
                   fit_transform = function(data){
                     self$fit(data)
                     self$transform(data)
                   }
                 )
)
