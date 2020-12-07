library(jsonlite)
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
#' @slot element_id integer, element id
#' @slot accession_ids vector of characters, accession ids
#'
#' Example to use this class
#'   > cube_obj = Cube$new()
#'   > cube_obj$login()    # call the login to get url for verification
#'   # do the verification in a browser
#'   > json = cube_obj$get_metadata_json(element_id = 116, page_size = 5)
#'   > metadata = cube_obj$get_metadata(element_id = 116, page_size = 5)
#'
#' @export
Cube = R6::R6Class(
  "Cube",
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  #' @description
  #' private fields
  private = list(
    .database_obj = NULL
  ),

  public = list(
    #' @field cubeapi \code{CubeAPI}, CubeAPI
    cubeapi = NULL,
    #' @field element_id integer, element id
    element_id = NULL,
    #' @field accession_ids vector of characters, accession ids
    accession_ids = NULL,

    #' @description
    #' Initialization method.
    #' @param element_id integer, element id
    #' @param accession_ids vector of characters, accession ids
    initialize = function(
      element_id = NULL,
      accession_ids = NULL
    ) {

      log_info(paste("Version", get_version(), sep = ": "))

      self$element_id = element_id
      self$accession_ids = accession_ids

      self$cubeapi = CubeAPI$new()

      # just keep it for now
      # private$.database_obj = Database$new()

    },

    #' @description
    #' get_version, get version information
    #'
    #' @return character, version information
    get_version = function() {
      CUBE_VERSION
    },

    #' @description
    #' login, call \code{CubeAPI} login
    #'
    #' @return character, url for verification, and user will open it in a browser
    login = function() {
      self$cubeapi$login()
    },

    #' @description
    #' get_metadata_json by calling cube app API and return json
    #' @param element_id integer, element id
    #' @param accession_ids vector of characters, accession ids
    #' @param page integer, page number
    #' @param page_size integer, number of items per page
    #'
    #' @return json \code{jsonlite::fromJSON}
    #'    > cube_obj = Cube$new()
    #'    > cube_obj$login()
    #'    > json = cube_obj$get_metadata_json(element_id = 116, page_size = 5)
    #'    > json$count
    #'    [1] 34
    #'    > class(json$results)
    #'    [1] "data.frame"
    #'    > head(json$results, 2)
    #'      id element.id element.element_label element.element_description element.element_collection_label element.element_abbreviation  element.icon_name
    #'       1 43465        116          Weight Study                        test                   Weight Studies                           WS stacked_line_chart
    #'       2 43464        116          Weight Study                        test                   Weight Studies                           WS stacked_line_chart
    #'       element.is_primary element_id
    #'       1               TRUE        116
    #'       2               TRUE        116
    #'
    get_metadata_json = function(
      element_id = NULL,
      accession_ids = NULL,
      page = 1,
      page_size = 100
    ) {
      response = self$get_metadata(element_id = element_id,
                                    accession_ids = accession_ids,
                                    page = page,
                                    page_size = page_size
                                    )
      response_json_to_data(response)
    },

    #' @description
    #' get_metadata by calling cube app API
    #' @param element_id integer, element id
    #' @param accession_ids vector of characters, accession ids
    #' @param page integer, page number
    #' @param page_size integer, number of items per page
    #'
    #' @return Response \code{httr::Response}  http response
    #'   > cube_obj = Cube$new()
    #'   > cube_obj$login()
    #'   > response = cube_obj$get_metadata(element_id = 116, page_size = 5)
    #'   > response$status_code          # get http status code
    #'   > content = content(response)   # get content of response, content is list
    #'   > names(content)
    #'      [1] "count"    "next"     "previous" "results"
    #'      > content$count
    #'      [1] 34
    #'      > content$results[1]
    #'      [[1]]
    #'      [[1]]$id
    #'      [1] 43465

    #'      [[1]]$element
    #'      [[1]]$element$id
    #'      [1] 116

    #'      [[1]]$element$element_label
    #'      [1] "Weight Study"

    #'      [[1]]$element$element_description
    #'      [1] "test"
    #'
    get_metadata = function(
      element_id = NULL,
      accession_ids = NULL,
      page = 1,
      page_size = 100
    ) {

      # this parameter takes priority, if not there, use constructor one
      if ( missing(element_id) ) {
        elem_id = self$element_id
      } else {
        elem_id = element_id
      }
      if ( missing(elem_id) ) {
        stop( paste("Missing parameter: element_id ", elem_id) )
      }

      # same for the accession ids
      if ( missing(accession_ids)) {
        acc_ids = self$accession_ids
      } else {
        acc_ids = accession_ids
      }

      results = self$cubeapi$get_element_instance(elem_id,
                        acc_ids, page, page_size)
    },

    #' @description
    #' get_metadata directly from Postgresql database, prefer to use API call
    #' keep it for now
    #' @param element_id integer, element id
    #' @param accession_ids vector of characters, accession ids
    #' @param page integer, page number
    #' @param page_size integer, number of items per page
    #' @param is_pivot boolean, to indicate if the return data
    #'    is pivoted by property_label
    #'
    #' @return data.frame
    get_metadata_from_database = function(
      element_id = NULL,
      accession_ids = NULL,
      page = 1,
      page_size = 100,
      is_pivot = FALSE
    ) {

      # this parameter takes priority, if not there, use constructor one
      if ( missing(element_id) ) {
        data_id = self$element_id
      } else {
        data_id = element_id
      }
      if ( missing(data_id) ) {
        stop( paste("Missing parameter: element_id ", element_id) )
      }

      # same for the accession ids
      if ( missing(accession_ids)) {
        acc_ids = self$accession_ids
      } else {
        acc_ids = accession_ids
      }

      if ( is_pivot ) {
        column_info = private$.database_obj$get_column_info(data_id)
        query = get_metadata_query_pivot(data_id, acc_ids,
            max_number_of_row, column_info)
      } else {
        query = get_metadata_query(data_id, acc_ids,
            max_number_of_row)
      }
      results = private$.database_obj$select(query);
    },

    #' @description
    #' get_element
    #' @param element_id integer, element id
    #'
    #' @return Response \code{httr::Response}  http response
    get_element = function(
      element_id = NULL
    ) {
      response = self$cubeapi$get_element(element_id)
    },

    #' @description
    #' get_data, get actual data
    #' @param page integer, page number
    #' @param page_size integer, number of items per page
    #'
    #' @return data.frame
    get_data = function(
      page = 1,
      page_size = 100
    ) {
      # to do
      log_info("get_data ", page_size)

    },

    #' @description
    #' annotate
    #' annotate derived data objects with a metadata description
    annotate = function(
    ) {
      # to do
      log_info("annotate derived data")
    },

    #' @description
    #' save_annotated_data
    #' save the annotated derived data object
    save_annotated_data = function(
    ) {
      # to do
      log_info("save_annotated_data")

    },

    #' @description
    #' test method.
    #' This is a test function
    test = function() {
      self$element_id = 85
      metadata = self$get_metadata(2, TRUE)
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
      cat("  element_id: ", self$element_id, "\n", sep = "")
      cat("  accession_ids: ", private$accession_ids, "\n", sep = "")
      invisible(self)
    }
  ),
  lock_objects = FALSE,
  lock_class = TRUE
)

