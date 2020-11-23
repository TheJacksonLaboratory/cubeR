library(logger)
log_threshold(DEBUG)

#' The Cube data object
#'
#' Class \code{cube} defines a Cube analysis object.
#'
#' @name Cube-class
#' @rdname Cube-class
#' @exportClass Cube
#'
#' @slot parameter_file string, full path to YAML file with initialization
#'   parameters
#'
#'
#' @export
Cube <- R6::R6Class(
  "Cube",
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  #' @description
  #' @field .database_obj \code{\link{Database}} object
  private = list(
    .database_obj = NULL
  ),

  public = list(
    #' @field dataset_id integer dataset id
    dataset_id = NULL,
    #' @field accession_ids a character vector of access ids
    accession_ids = NULL,

    #' @description
    #' Initialization method.
    #' @param dataset_id integer dataset id
    #' @param accession_ids a character vector of access ids
    initialize = function(
      dataset_id = NULL,
      accession_ids = NULL
    ) {
      self$dataset_id <- dataset_id
      self$accession_ids <- accession_ids

      private$.database_obj <- Database$new()

    },

    #' @description
    #' get_metadata
    #' @param dataset_id integer dataset id
    #' @param accession_ids a character vector of access ids
    #' @param max_number_of_row maximum number of rows returned.
    #'    default is 100 and -1 for everything
    #' @param is_pivot boolean to indicate if the return data
    #'    is pivoted by property_label
    get_metadata = function(
      dataset_id = NULL,
      accession_ids = NULL,
      max_number_of_row = 100,
      is_pivot = FALSE
    ) {

      # this parameter takes priority, if not there, use constructor one
      if ( missing(dataset_id) ) {
        data_id = self$dataset_id
      } else {
        data_id = dataset_id
      }
      if ( missing(data_id) ) {
        log_error( paste("Missing parameter: dataset_id ", dataset_id) )
        stop()
      }

      # same for the accession ids
      if ( missing(accession_ids)) {
        acc_ids = self$accession_ids
      } else {
        acc_ids = accession_ids
      }

      if ( is_pivot ) {
        column_info <- private$.database_obj$get_column_info(data_id)
        query <- get_metadata_query_pivot(data_id, acc_ids,
            max_number_of_row, column_info)
      } else {
        query <- get_metadata_query(data_id, acc_ids,
            max_number_of_row)
      }
      results <- private$.database_obj$select(query);
    },

    #' @description
    #' get_data
    #' @param max_number_of_row maximum number of rows returned.
    #'     default is 100 and -1 for everything
    get_data = function(
      max_number_of_row = 50
    ) {
      log_info("get_data ", max_number_of_row)

    },

    #' @description
    #' annotate
    #' annotate derived data objects with a metadata description
    annotate = function(
    ) {
      log_info("annotate ", max_number_of_row)

    },

    #' @description
    #' save_annotated_data
    #' save the annotated derived data object
    save_annotated_data = function(
    ) {
      log_info(DEBUG)("annotate ", max_number_of_row)

    },

    #' @description
    #' test method.
    #' This is a test function
    test = function() {
      self$dataset_id = 85
      metadata <- self$get_metadata(2, TRUE)
      metadata
    },

    #' @description
    #' finalize
    finalize = function() {
      log_info("Cleaning up ", self$path)
      unlink(self$path)
    },

    #' @description
    #' print
    print = function() {
      cat("Cube: \n")
      cat("  dataset_id: ", self$dataset_id, "\n", sep = "")
      cat("  accession_ids: ", private$accession_ids, "\n", sep = "")
      invisible(self)
    }
  ),
  lock_objects = FALSE,
  lock_class = TRUE
)

