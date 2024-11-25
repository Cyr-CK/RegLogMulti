#' Factor Analysis of Mixed Data
#'
#' @description
#' Class of a FAMD object that has fit, fit_transform and transform methods to embed the qualitative and quantitative features into a new quantitative coordinates system
#'
#' @details
#'
#' @import R6
#' @import FactoMineR
#' @export
#'
#' @examples
#' famd <- FAMD_$new()
#'
#' X_train_famd <- famd$fit_transform(X_train)
#'
#' famd$fit(X_train)
#' X_test_famd <- famd$transform(X_test)

FAMD_ <- R6::R6Class("FAMD_",
                 public = list(
                   #' @field model FAMD object
                   model = NULL,
                   #' @field n_components Number of components to keep from the new coordinates system
                   n_components = NULL,

                   #' @description
                   #' Create a new FAMD object
                   #' @param n_components Number of components
                   #' @return A new FAMD object
                   initialize = function(n_components = 2){
                     self$n_components <- n_components
                   },

                   #' @description
                   #' Fit the FAMD model to data
                   #' @param data Dataframe
                   fit = function(data){
                     if (!is.data.frame(data)){
                       stop("Data are not in a dataframe format")
                     }
                     self$model <- FactoMineR::FAMD(data, ncp=self$n_components, graph=FALSE)
                     invisible(self)
                   },

                   #' @description
                   #' Transform the passed data object based on the fit data object
                   #' @param data Dataframe
                   #' @return Dataframe of new data coordinates
                   transform = function(data){
                     if (is.null(self$model)){
                       stop("Fit() method must be used before transform()")
                     }
                     coord <- predict(self$model, data)$coord

                     return(as.data.frame(coord[, 1:self$n_components]))
                   },

                   #' @description
                   #' Fit to data object then model it with FAMD
                   #' @param data Dataframe
                   #' @return Dataframe of new data coordinates
                   fit_transform = function(data){
                     self$fit(data)
                     self$transform(data)
                   }
                 ),
)
