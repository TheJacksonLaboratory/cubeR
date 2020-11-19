library(httr)
library(jsonlite)

#' CubeAPI
#'
#'
#' Class \code{CubeAPI} defines a CubeAPI object to interact with Cube Web API.
#'
#' @name CubeAPI-class
#' @rdname CubeAPI-class
#' @exportClass CubeAPI
#'
#'
#'
#' @export
CubeAPI <- R6::R6Class(
  "CubeAPI",
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  #' @description
  #' @field .token api call token
  private = list(
    .token = NULL,
    .endpoint_element_instance = "metadata_repository/element_instance",
    .endpoint_disclaimer = "cube_about/disclaimer/"
  ),
  public = list(
    #' @field cube web app API base
    url_base = NULL,

    #' @description
    #' Initialization method.
    #' @param url_base cube web app API base

    initialize = function(
      url_base = NULL
    ) {
      if (missing(url_base)) self$url_base <- Sys.getenv("CUBE_APP_API_URL_BASE")
    },

    #' @description
    #' get method.
    #' @param endpoint a get endpoint to call
    #'  required
    #' @param limit max number of data returned
    #'
    #' @return Response \code{httr::Response}
    #' https://httr.r-lib.org
    get = function(endpoint, limit=0) {
      url = paste0(endpoint, "?format=json")
      if ( limit > 0 ) {
        url = paste0(url, "&limit=", limit)
      }
      log_debug(url)

      # need to add user authentication
      #response <- GET(url, authenticate("xxxx", "xxxx"))
      response <- GET(url)
      log_debug(paste0("status_code: ", response$status_code))
      response
    },


    #' @description
    #' get_element_instance method.
    #' @param limit max number of data returned
    #'
    #' @return Response \code{httr::Response}
    get_element_instance = function(limit = 100) {
      endpoint <- paste0(self$url_base, private$.endpoint_element_instance);
      self$get(endpoint, limit)
    },

    #' @description
    #' get_disclaimer method.
    #'
    #' @return Response \code{httr::Response}
    get_disclaimer = function() {
      endpoint <- paste0(self$url_base, private$.endpoint_disclaimer);
      self$get(endpoint)
    },


    #' @description
    #' get_disclaimer_json method.
    #'
    #' @return json \code{jsonlite::json}
    get_disclaimer_json = function() {
      response <- self$get_disclaimer()
      content <- content(response, "text")
      json <- fromJSON(content)

      #result is data.frame
      results <- json$results

      log_info(paste("id", results[1, "id"], sep = ": "))
      log_info(paste("activated_date", results[1, "activated_date"], sep = ": "))
      log_info(paste("terms_text", results[1,"terms_text"], sep = ": "))

      json
    },

    #' @description
    #' finalize
    finalize = function() {
      log_info("finalize", self$path)
    },

    #' @description
    #' print
    print = function() {
      cat("CubeAPI: \n")
      cat("  url_base: ", self$url_base, "\n", sep="")
      cat("  .token: ", private$.token, "\n", sep="")
      invisible(self)
    }
  ),

  lock_objects = FALSE,
  lock_class = TRUE
)



