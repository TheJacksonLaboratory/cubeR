library(httr)
library(jsonlite)
library(stringr)
library(rapportools)

#' CubeAPI
#'
#' Class \code{CubeAPI} defines a CubeAPI object to interact with Cube Web API.
#'
#' usage example:
#'   > cube_api = CubeAPI$new()
#'   > cube_api$login()
#'   > storage_info <- cube_api$get_metadata_collection_storage_info()
#'   > View(storage_info)
#'   > data <- get_bucket_data(bucket_name = storage_info[2, 3], file_name = storage_info[2, 4])
#'   > response = cube_api$get_element()
#'
#' @name CubeAPI-class
#' @rdname CubeAPI-class
#' @exportClass CubeAPI
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
      log_info("Version: {get_version()}")
      if (missing(url_base))
        self$url_base <- Sys.getenv("CUBE_APP_API_URL_BASE")
      else
        self$url_base <- url_base
      log_info(self$url_base)

      self$auth0_obj <- Auth0DeviceAuth$new()
    },

    #' @description
    #' login, call this method to login auth0
    #'
    #' @return character verification_uri and user_code
    login = function() {
      response <- self$auth0_obj$get_device_code()
      verification_uri <- self$auth0_obj$verification_uri_complete

      # start the login widget, only if run within RStudio
      if ( Sys.getenv("RSTUDIO") == "1" ) {
        login_widget(verification_uri)
      }
    },

    #' @description
    #' get_element
    #' @param element_id integer, element id
    #'
    #' @return Response \code{httr::Response}  http response
    get_element = function(
      element_id = NULL
    ) {
      end_point <- self$append_to_path( end_point = API_END_POINT_metadata_definition_element,
                                       path = element_id)
      response <- self$get_end_point(end_point = end_point)
    },

    #' @description
    #' get_metadata_collection  get metadata collection
    #'
    #' @return Response \code{httr::Response}  http response
    get_metadata_collection = function(
    ) {
      self$get_end_point(API_END_POINT_metadata_repository_collection)
    },

    #' @description
    #' get_metadata_collection_storage_info  get metadata collection bucket
    #' storage information
    #'
    #' @return dataframe with 4 columns:
    #'  "accession_id", "uri", "bucket_name", "file_name"
    get_metadata_collection_storage_info = function(
    ) {
      response <- self$get_metadata_collection()
      if ( response$status_code != 200 ) {
        stop("Failed to get_metadata_collection")
      }
      accession_ids <- self$parse_accession_ids(response = response)
      log_info("number of accession_ids: {length(accession_ids)}")
      if ( length(accession_ids) < 1 ) {
        stop("No collection data found")
      }

      md_response <- self$get_element_instance(accession_ids =
                                unique(accession_ids, incomparables = FALSE))
      if ( md_response$status_code != 200 ) {
        stop("Failed to get metadata for accession ids")
      }

      df <- self$parse_storage_uri(md_response)
      log_info("number of row returned: {nrow(df)}")
      return (df)
    },

    #' @description
    #' parse_accession_ids  parse out accession ids
    #' @param response Response \code{httr::Response}  http response
    #'
    #' @return vector of characters, list of accession id
    parse_accession_ids = function(response) {
      results <- content(response)$results
      accession_ids <- vector("list")
      index <- 1
      for (i in 1:length(results)) {
        result <- results[[i]]
        if (!exists('collection_items', where=result)) {
          next
        }
        for (collection_item in result['collection_items'] ) {
          if ( length(collection_item) > 0 ) {
            for (j in 1:length(collection_item)) {
                accession_ids[[index]] <- (collection_item[[j]]$accession_id )
                index <- index + 1
            }
          }
        }
      }
      return( Filter(Negate(is.null), accession_ids) )
    },


    #' @description
    #' parse_storage_uri  parse out storage url information
    #' @param response Response \code{httr::Response}  http response
    #'
    #' @return dataframe with 4 columns:
    #'  "accession_id", "uri", "bucket_name", "file_name"
    parse_storage_uri = function(response) {
      results <- content(response)$results
      df <- data.frame("accession_id","uri", "bucket_name", "file_name")
      names(df)<-c("accession_id","uri", "bucket_name", "file_name")
      index <- 1
      for (i in 1:length(results)) {
        result <- results[[i]]
        if (exists('property_values', where=result)) {
          uri <- Filter(
            function(x) !is.empty(x$property_value) && (startsWith(x$property_value, 'http')
                || startsWith(x$property_value, 'gs')) ,
            result$property_values)

          #log_info("Accession_id: {result$accession_id}, url: {result$property_values}")
          if (length(uri) > 0) {
            value = uri[[1]]$property_value
            bucket_file <- self$parse_uri(value)
            df[index,] = list(accession_id = result$accession_id,
                              uri = value,
                              bucket_name = bucket_file$bucket_name,
                              file_name = bucket_file$file_name)
            index <- index + 1
          } else {
            log_info("Accession_id: {result$accession_id}, url: {uri}")
          }
        }
      }
      return (df)
    },

    #' @description
    #' parse_uri  parse out url list
    #'
    #' @param uri character, uri
    #'   example: https://storage.cloud.google.com/jax-cube-prd-ctrl-01-project-data/study_4-1/4.1_DEXA_to_201016.txt?authuser=1
    #'
    #' @return vector of characters, list(bucket_name, file_name)
    parse_uri = function(uri) {
      if ( length(uri) < 1 ) {
        return (c("", ""))
      }

      parts <- str_split(uri, "/")[[1]]
      len <- length(parts)
      if ( len < 1 ) {
        return (list(bucket_name = "", file_name = ""))
      }

      # concacenate bucket name
      start_index <- 4
      if ( startsWith(uri, "http") ) {
        start_index <- 5
      }
      bucket_name <- "gs://"


      for (i in start_index:len-1) {
        bucket_name = paste0(bucket_name, parts[i])
        if ( i < len-1 ) {
          bucket_name = paste0(bucket_name, "/")
        }
      }
      #bucket_name = paste0("gs://", parts[len-2], "/", parts[len-1])

      # check last part if it is a file name, just by . for now
      if ( grepl(".", parts[len], fixed=TRUE) ) {
        # file name is last part
        file_name <- parts[len]
        # remove "?authuser=1" from file name if exist
        file_name <- str_split(file_name, "\\?")[[1]][1]
      } else {
        file_name = ""
        bucket_name = paste0(bucket_name, "/", parts[len])
      }

      list(bucket_name = bucket_name, file_name = file_name)
    },


    #' @description
    #' get_data_store_files
    #' @param dir_name character, directory name
    #'   required
    #'
    #' @return Response \code{httr::Response}  http response
    get_data_store_files = function(
      dir_name
    ) {
      end_point <- self$append_to_path(end_point = API_END_POINT_data_store_files,
                                      path = dir_name)
      response <- self$get_end_point(end_point = end_point)
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
      self$get_end_point(end_point =API_END_POINT_metadata_repository_element_instance,
                         element_id = element_id,
                         accession_ids = accession_ids,
                         page = page,
                         page_size = page_size)
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
      response <- self$get_element_instance( element_id = element_id,
                                            accession_ids = accession_ids,
                                            page = page,
                                            page_size = page_size)
      response_json_to_data(response)
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
      url <- paste0(self$url_base, end_point);
      query <- list(element_id = element_id,
                   page = page,
                   page_size = page_size)
      if ( length(accession_ids) > 0 ) {
        for (id in accession_ids) {
          query <- c(query, accession_id=id)
        }
      }
#      names(accession_ids) = rep(c("accession_id "),times=length(accession_ids))
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
    #'
    #'
    #' @param element_id integer, element id
    #'   required
    #' @param parent_element_instance_id integer, parent element instance id
    #' @param accession_ids vector of characters, accession ids
    #' @param property_filters vector of characters,
    #' @param page integer, page number
    #' @param page_size integer, number of items per page
    #'
    #' @return Response \code{httr::Response}
    post_element_instance_filter = function(
      element_id,
      parent_element_instance_id = NULL,
      accession_ids = NULL,
      property_filters = NULL,
      page = 1,
      page_size = 100
    ) {

      body <- toJSON(list(elementId = element_id,
                         parent_element_instance_id = parent_element_instance_id,
                         accessionId = accession_ids,
                         propertyFilters = property_filters
                         ), auto_unbox = TRUE)

      self$post_end_point(end_point = API_END_POINT_metadata_repository_element_instance_filter,
                          body = body,
                          page = page,
                          page_size = page_size)
    },

    #' @description
    #' post_end_point method. post to a end point
    #'    eg: "metadata_repository/element_instance/filter"
    #' @param end_point character,  API end point to call,
    #'   required
    #' @param body character, body payload,
    #'   required
    #'   eg. {"elementId": 122, "propertyFilters": []}
    #' @param page integer, page number
    #' @param page_size integer, number of items per page
    #'
    #' @return Response \code{httr::Response}
    post_end_point = function(
      end_point,
      body,
      page = 1,
      page_size = 100
    ) {
      url <- paste0(self$url_base, end_point)
      query <- list(page = page,
                   page_size = page_size)
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
    #'  for example: query = list(element_id = "85", accession_id = "JAXAS0005k")
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
      if ( is.empty(self$auth0_obj$access_token) ) {

        # check if device_code exisit, if not, stop and ask user to run login first
        if ( is.empty(self$auth0_obj$device_code) ) {
          stop("Unauthorized, run 'cube_api$login()' to login")
        }

        self$auth0_obj$get_access_token()

      }

      # if no access token, stop here
      if ( is.empty(self$auth0_obj$access_token) )
        stop( paste0("Verification error: open the following url to verify: ",
                    self$auth0_obj$verification_uri_complete))

      # to do:  need to check the expiration of token
      log_info(paste(HTTP_METHOD_LABELS[method], url, sep = ": "))
      if ( method == HTTP_METHOD_POST ) {
        # add with_verbose to debug
        response <- POST(url, query = query, body = body, add_headers(
          "Authorization" = paste("Bearer", self$auth0_obj$access_token, sep = " "),
          "Content-Type"  = "application/json"),
          encode = "json")   # either raw or json works
      } else {
        response <- GET(url, query = query, add_headers(
          "Authorization" = paste("Bearer", self$auth0_obj$access_token, sep = " "),
          "Content-Type"  = "application/json"))
      }

      log_info("status_code: {response$status_code}")
      log_debug("response url: {response$url}")
      if ( response$status_code != 200 ) {
        content = content(response)
        log_error("response: {content} ")
        log_error("response url: {response$url}")
        # check for access token expire
        if ( grepl("Signature has expired", content, fixed = TRUE ) ) {
          log_error("Token expired. run 'cube_api$login()' to login")
          #clear out expired access token
          self$auth0_obj$clear_access_token()
          # relogin
          self$login()
        # check invalid token
        } else if ( grepl("Error decoding signature", content, fixed = TRUE ) ) {
          log_error("Invalid Token. run 'cube_api$login()' to login")
          #clear out expired access token
          self$auth0_obj$clear_access_token()
          # relogin
          self$login()
        }
      }
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
    #' append_to_path, append an id to a end point, for example
    #'   pass in   "metadata_definition/element"    35
    #'   to return  "metadata_definition/element/35"
    #' @param end_point character, end point to be append
    #'   required
    #' @param path integer/character, to be appended
    #'   required
    #'
    #' @return character end point with id appended
    append_to_path = function(
      end_point,
      path
    ) {
      ep = end_point
      if ( !is.empty(path) ) {
        ep = paste(ep, path, sep = "/")
      }
      ep
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



