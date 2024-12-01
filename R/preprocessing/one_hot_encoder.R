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

OneHotEncoder <- R6::R6Class("OneHotEncoder",
  public = list(
    categories = NULL,    # Liste des catégories pour chaque colonne qualitative
    drop_last = FALSE,    # Option pour supprimer la dernière catégorie
    sparse = TRUE,        # Option pour retourner une matrice creuse

    #' @description
    #' Constructeur pour la classe One.Hot.Encoder
    #' @param drop_last Logique indiquant s'il faut supprimer la dernière catégorie
    #' @param sparse Logique indiquant si la matrice encodée doit être creuse
    initialize = function(drop_last = FALSE, sparse = TRUE) {
      self$drop_last <- drop_last
      self$sparse <- sparse
    },
    
    #' @description
    #' Cette méthode ajuste l'encodeur One-Hot sur le jeu de données fourni.
    #' @param data Un data.frame contenant les données qualitatives à encoder.
    #' @return L'instance de l'encodeur avec les catégories apprises.
    fit = function(data) {
      if (!is.data.frame(data)){
        stop("`data` doit être un data.frame. Utilisez as.data.frame() si nécessaire.")
      }
      
    # Identifier les colonnes qualitatives
    quali <- names(data)[!sapply(data, function(x) is.numeric(x) || is.integer(x))]
         
      
      # Apprendre les catégories uniques pour chaque colonne qualitative
      self$categories <- list()
      for (col in quali) {
        self$categories[[col]] <- unique(data[[col]])
      }
      
      invisible(self)
    },
    
    #' @description
    #' Cette méthode transforme un jeu de données en appliquant l'encodage One-Hot appris.
    #' @param data Un data.frame contenant les données qualitatives à encoder.
    #' @return Une matrice creuse (sparse) ou dense contenant les caractéristiques encodées.
transform = function(data) {
  if (is.null(self$categories)){
    stop("La méthode fit() doit être utilisée avant transform().")
  }
  
  if (!is.data.frame(data)){
    stop("`data` doit être un data.frame. Utilisez as.data.frame() si nécessaire.")
  }
  
  # Convertir data en data.frame si ce n'est pas déjà le cas (éviter les data.tables)
  data <- as.data.frame(data)
  
  # Identifier les colonnes qualitatives présentes dans les données à transformer
  quali <- intersect(names(data)[!sapply(data, is.numeric)], names(self$categories))
  
  if (length(quali) == 0 && !self$drop_last) {
    warning("Aucune colonne qualitative à encoder.")
  }
  
  # Liste pour stocker les matrices encodées
  encoded_list <- list()
  
  for (col in quali) {
    # Assurer que les catégories dans les nouvelles données sont connues
    data[[col]] <- factor(data[[col]], levels = self$categories[[col]])
    
    # Optionnel: Remplacer les NA par une catégorie "Unknown"
    if (any(is.na(data[[col]]))) {
      data[[col]] <- addNA(data[[col]])
      levels(data[[col]])[is.na(levels(data[[col]]))] <- "Unknown"
      warning(paste("Des catégories inconnues trouvées dans la colonne", col, ". Elles seront encodées comme 'Unknown'."))
    }
    
    # Créer la formule dynamiquement en utilisant reformulate pour gérer les noms avec espaces
    formula <- reformulate(termlabels = col, intercept = FALSE)
    
    # Utiliser sparse.model.matrix pour créer une matrice creuse
    encoded <- sparse.model.matrix(formula, data = data[, col, drop = FALSE])
    
    # Vérifier si l'encodage a réussi
    if (ncol(encoded) == 0) {
      warning(paste("Aucune colonne encodée pour", col))
      next
    }
    
    # Assurer que les noms des colonnes encodées sont corrects
    colnames(encoded) <- paste(col, levels(data[[col]]), sep = "_")
    
    # Option pour supprimer la dernière catégorie afin d'éviter la multicolinéarité
    if (self$drop_last && ncol(encoded) > 1) {
      encoded <- encoded[, -ncol(encoded), drop = FALSE]
    }
    
    encoded_list[[col]] <- encoded
  }
  
  # Supprimer les colonnes qualitatives originales
  data <- data[, !names(data) %in% quali, drop = FALSE]
  
  # Convertir les colonnes numériques en matrice sparse si nécessaire
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  if (length(numeric_cols) > 0) {
    message("Encodage des colonnes numériques...")
    numeric_matrix <- as.matrix(data[, numeric_cols, drop = FALSE])
    
    if (self$sparse) {
      numeric_matrix <- Matrix::Matrix(numeric_matrix, sparse = TRUE)
    } else {
      numeric_matrix <- as.matrix(numeric_matrix)
    }
    
    encoded_list[["numeric"]] <- numeric_matrix
  }
  
  # Combiner les matrices encodées sans convertir en dense et en évitant les erreurs de sélection
  if (length(encoded_list) > 0) {
    # Utiliser Matrix::cBind pour conserver le format sparse
    encoded_combined <- Reduce(function(x, y) cbind(x, y), encoded_list)
    return(encoded_combined)
  }
  
  stop("Aucune donnée encodée à retourner.")
},
    
    #' @description
    #' Cette méthode ajuste puis transforme le jeu de données en une seule étape.
    #' @param data Un data.frame contenant les données qualitatives à encoder.
    #' @return Une matrice creuse (sparse) ou dense contenant les caractéristiques encodées.
    fit_transform = function(data) {
      self$fit(data)
      return(self$transform(data))
    }
  )
)
