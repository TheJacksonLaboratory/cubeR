library(httr)
library(jsonlite)
library(stringr)

#' CubeAPI
#'
#' Class \code{CubeAPI} defines a CubeAPI object to interact with Cube Web API.
#'
#' @name CubeAPI-class
#' @rdname CubeAPI-class
#' @exportClass CubeAPI
#'
#' usage example:
#'   > cube_api = CubeAPI$new()
#'   > cube_api$login()
#'   > response = cube_api$get_element()
#'
#' @export
CubeAPI <- R6::R6Class(
  "CubeAPI",
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  #' @description
  #' private fields
  private = list(
  ),

  #' @description
  #' public fields
  public = list(
    #' @field url_base character, cube web app API base
    url_base = NULL,

    #' @field auth0_obj \code{Auth0DeviceAuth} an instance of Auth0DeviceAuth
    auth0_obj = NULL,

    #' @description
    #' Initialization method.
    #' @param url_base character, cube web app API base, if not existing, take
    #' from environmental variable

    initialize = function(
      url_base = NULL
    ) {
      if (missing(url_base))
        self$url_base = Sys.getenv("CUBE_APP_API_URL_BASE")
      else
        self$url_base = url_base
      log_debug(self$url_base)

      self$auth0_obj = Auth0DeviceAuth$new()

      # set access_token in environment variable if exists for testing purpose
      self$auth0_obj$access_token = Sys.getenv("TEST_ACCESS_TOKEN")
    },

    #' @description
    #' login, call this method to login auth0
    #'
    #' @return character verification_uri and user_code
    login = function() {
      response = self$auth0_obj$get_device_code()
      self$auth0_obj$verification_uri_complete
    },

    #' @description
    #' get_element
    #'
    #' @return Response \code{httr::Response}  http response
    get_element = function(
    ) {
      response = self$get_end_point(API_END_POINT_metadata_definition_element)
    },

    #' @description
    #' get_element_instance method.
    #' @param element_id integer, element id
    #' @param accession_ids vector of characters, accession ids
    #' @param page integer, page number
    #' @param page_size integer, number of items per page
    #'
    #' @return Response \code{httr::Response}
    get_element_instance = function(
      element_id = NULL,
      accession_ids = NULL,
      page = 1,
      page_size = 100
    ) {
      self$get_end_point(API_END_POINT_metadata_repository_element_instance,
                         element_id, accession_ids, page, page_size)
    },

    #' @description
    #' get_element_instance_json method.
    #' @param element_id integer, element id
    #' @param accession_ids vector of characters. accession ids
    #' @param page integer, page number
    #' @param page_size integer, number of items per page
    #'
    #' @return json
    get_element_instance_json = function(
      element_id = NULL,
      accession_ids = NULL,
      page = 1,
      page_size = 100
    ) {
      response = self$get_element_instance(element_id, accession_ids,
                                            page, page_size)
      content = content(response, "text")
      json = fromJSON(content)
    },

    #' @description
    #' get_end_point method. get data by calling a end point
    #'    eg: "metadata_repository/element_instance"
    #' @param end_point character,  API end point to call,
    #'   required
    #' @param element_id integer, element id
    #' @param accession_ids vector of characters, accession ids
    #' @param page integer, page number
    #' @param page_size integer, number of items per page
    #'
    #' @return Response \code{httr::Response}
    get_end_point = function(
      end_point,
      element_id = NULL,
      accession_ids = NULL,
      page = 1,
      page_size = 100
    ) {
      url = paste0(self$url_base, end_point);

      query = list(element_id = element_id,
                   accession_id = accession_ids,
                   page = page,
                   page_size = page_size)

      self$call(method = HTTP_METHOD_GET, url = url, query = query)
    },

    #' @description
    #' post_element_instance_filter method. post call instance with filter
    #'  Action end point used for retrieving data based on a filter
    #'  The filter is passed in the request body, with the following format:
    #'    elementId: number;
    #'    parentElementInstanceId: number;
    #'    accessionId: string;
    #'    propertyFilters: [
    #'      propertyId: number;
    #'      dataType: string;
    #'      propertyValue: string;
    #'    ]
    #'
    #' @param element_id integer, element id
    #' @param parent_element_instance_id integer, parent element instance id
    #' @param accession_ids vector of characters, accession ids
    #' @param page integer, page number
    #' @param page_size integer, number of items per page
    #'
    #' @return Response \code{httr::Response}
    post_element_instance_filter = function(
      element_id = NULL,
      parent_element_instance_id = NULL,
      accession_ids = NULL,
      page = 1,
      page_size = 100
    ) {
      self$post_end_point(API_END_POINT_metadata_repository_element_instance_filter,
                         element_id, accession_ids, page, page_size)
    },

    #' @description
    #' post_end_point method. post to a end point
    #'    eg: "metadata_repository/element_instance/filter"
    #' @param end_point character,  API end point to call,
    #'   required
    #' @param element_id integer, element id
    #' @param parent_element_instance_id integer, parent element instance id
    #' @param accession_ids vector of characters, accession ids
    #' @param property_filters vector of property filters in format
    #'    propertyFilters: [
    #'      propertyId: number;
    #'      dataType: string;
    #'      propertyValue: string;
    #'    ]
    #' @param page integer, page number
    #' @param page_size integer, number of items per page
    #'
    #' @return Response \code{httr::Response}
    post_end_point = function(
      end_point,
      element_id = NULL,
      parent_element_instance_id = NULL,
      accession_ids = NULL,
      property_filters = NULL,
      page = 1,
      page_size = 100
    ) {
      url = paste0(self$url_base, end_point);

      query = list(page = page,
                   page_size = page_size)


      body = list(element_id = element_id,
                  parentElementInstanceId = parent_element_instance_id,
                  accession_id = accession_ids,
                  propertyFilters = property_filters
                  )
      self$call(method = HTTP_METHOD_POST, url = url, query = query, body = body)
    },


    #' @description
    #' call method. call url and return a response, based on the method provided
    #' @param method integer, a number to indicate which method to use
    #'  defined in constants.R
    #'  required
    #' @param url character, a url to call
    #'  required
    #' @param query character, a key value list
    #'  for example: query = list(element_id = "85", accession_id = ["", ""])
    #'
    #' @param body list, post body
    #'
    #' @return Response \code{httr::Response}
    #' https://httr.r-lib.org
    call = function(method,
                    url,
                    query = NULL,
                    body = NULL) {

      # get access token. if is not here, get it on fly
      if ( is.null(self$auth0_obj$access_token) ) {

        # check if device_code exisit, if not, stop and ask user to run login first
        if ( is.null(self$auth0_obj$device_code) ) {
          stop("Unauthorized, please call login to get verification URL and verify
             it in a browser")
        }

        log_debug("call auth0_obj$get_access_token()")
        self$auth0_obj$get_access_token()
      }

      # if no access token, stop here
      if ( is.null(self$auth0_obj$access_token) )
        stop( paste0("Validation error: Please open the validate URL in the the browser: ",
                    self$auth0_obj$verification_uri_complete))

      # to do:  need to check the expiration of token
      log_debug(paste(method, url, sep = ", "))
      if ( method == HTTP_METHOD_POST ) {
        # add with_verbose to debug
        response = POST(url, query = query, body = body, httr::add_headers(
          "Authorization" = paste("Bearer", self$auth0_obj$access_token, sep = " "),
          "Content-Type"  = "application/json"),
          encode = "form")
      } else {
        response = GET(url, query = query, httr::add_headers(
          "Authorization" = paste("Bearer", self$auth0_obj$access_token, sep = " "),
          "Content-Type"  = "application/json"))
      }

      log_debug(paste0("status_code: ", response$status_code))
      response
    },

    #' @description
    #' get_disclaimer method.
    #'
    #' @return Response \code{httr::Response}
    get_disclaimer = function() {
      self$get_end_point(API_END_POINT_cube_about_disclaimer)
    },


    #' @description
    #' get_disclaimer_json method.
    #'
    #' @return json \code{jsonlite::json}
    get_disclaimer_json = function() {
      response = self$get_disclaimer()
      content = content(response, "text")
      json = fromJSON(content)

      #result is data.frame
      results = json$results

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
      cat("  .access_token: ", private$.access_token, "\n", sep="")
      invisible(self)
    }
  ),

  lock_objects = FALSE,
  lock_class = TRUE
)



