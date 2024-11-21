library(R6)
# OHE
library(caret)
one.hot.encoder <- R6Class("one.hot.encoder",
                   public = list(
                       categories = NULL,
                       
                       initialize = function(){
                           self$categories <- list()
                       },
                       
                       fit = function(data) {
                           for (col in names(data)) {
                               self$categories[[col]] <- unique(data[[col]])
                           }
                           invisible(self)
                       },
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
                       
                       fit_transform = function(data) {
                           self$fit(data)
                           self$transform(data)
                       }
                       
                   )
                   )


library(FactoMineR)
# library(factoextra)
afdm <- R6Class("afdm",
        public = list(
            model = NULL,
            n_components = NULL,
            
            initialize = function(n_components = 2){
                self$n_components <- n_components
            },
            fit = function(data){
                if (!is.data.frame(data)){
                    stop("Data are not in a dataframe format")
                }
                self$model <- FAMD(data, ncp=self$n_components, graph=FALSE)
                invisible(self)
            },
            transform = function(data){
                if (is.null(self$model)){
                    stop("Fit() method must be used before transform()")
                }
                coord <- predict(self$model, data)$coord
                
                return(as.data.frame(coord[, 1:self$n_components]))
            },
            
            fit_transform = function(data){
                self$fit(data)
                self$transform(data)
            }
        ),
)


# quanti NA imputer
SimpleQuantiImputer <- R6Class("SimpleQuantiImputer",
                       public = list(
                           imputation_type = NULL,
                           imputation_values = NULL,
                           
                           initialize = function(imputation_type = "mean") {
                               self$imputation_type <- imputation_type
                           },
                           
                           fit = function(data, column) {
                               self$imputation_values <- apply(data[column], 2, function(x) { 
                                   if (self$imputation_type == "mean") {
                                       mean(x, na.rm = TRUE)
                                   } else if (self$imputation_type == "median") {
                                       median(x, na.rm = TRUE)
                                   } else {
                                       stop("Unsupported imputation type")
                                   }
                               }
                               )
                           },
                           
                           transform = function(data) {
                               for (col in names(self$imputation_values)) {
                                   data[[col]][is.na(data[[col]])] <- self$imputation_values[[col]]
                               }
                               return(data)
                           },
                           
                           fit_transform = function(data, column) {
                               self$fit(data, column)
                               return(self$transform(data))
                           }
                       )
)

# Quali NA imputer

SimpleQualiImputer <- R6Class("SimpleQualiImputer",
                       public = list(
                           imputation_type = NULL,
                           imputation_values = NULL,
                           
                           initialize = function(imputation_type = "mode") {
                               self$imputation_type <- imputation_type
                           },
                           
                           fit = function(data, column) {
                               self$imputation_values <- apply(data[column], 2, function(x) { 
                                   if (self$imputation_type == "mode") {
                                       Mode <- function(x, na.rm = FALSE) {
                                           if (na.rm) {
                                               x <- x[!is.na(x)]
                                           }
                                           unique_values <- unique(x)
                                           frequencies <- tabulate(match(x, unique_values))
                                           mode_values <- unique_values[frequencies == max(frequencies)]
                                           return(mode_values)
                                       }
                                       
                                       Mode(x, na.rm = TRUE)
                                   } else if (self$imputation_type == "probability") {
                                       # Remove NA values for probability calculation
                                       non_na_values <- x[!is.na(x)]
                                       
                                       # Calculate probability distribution
                                       prob_dist <- table(non_na_values) / length(non_na_values)
                                       
                                       # Impute missing values
                                       sample(
                                           names(prob_dist),
                                           sum(is.na(x)),
                                           replace = TRUE,
                                           prob = prob_dist
                                       )
                                   } else {
                                       stop("Unsupported imputation type")
                                   }
                               }
                               )
                           },
                           
                           transform = function(data) {
                               for (col in names(self$imputation_values)) {
                                   data[[col]][is.na(data[[col]])] <- self$imputation_values[[col]]
                               }
                               return(data)
                           },
                           
                           fit_transform = function(data, column) {
                               self$fit(data, column)
                               return(self$transform(data))
                           }
                       )
)

# IQR Outlier remover
