library(httr)

#' Auth0DeviceAuth
#'
#'
#' Class \code{Auth0DeviceAuth} defines a Auth0DeviceAuth object to interact with Cube Web API.
#'
#' @name Auth0DeviceAuth-class
#' @rdname Auth0DeviceAuth-class
#' @exportClass Auth0DeviceAuth
#'
#' 3 Steps for device authorization:
#'   1. call "get_device_code" to get device code, user verification url
#'      and user code.
#'      The user will open the verifcation url in a browser and enter the
#'      user code
#'   2. call the "get_access_token" with device_code to retrieve the access
#'      token
#'   3. call the API with access_token in the header
#'
#' Example call:
#' > auth0 = Auth0DeviceAuth$new()
#' > response = auth0$get_device_code()
#'   INFO [2020-12-01 14:02:56] verification_uri: https://thejacksonlaboratory.auth0.com/activate
#'   INFO [2020-12-01 14:02:56] verification_uri_complete: https://thejacksonlaboratory.auth0.com/activate?user_code=KCDG-SHDD
#'   INFO [2020-12-01 14:02:56] user_code: KCDG-SHDD
#'   INFO [2020-12-01 14:02:56] device_code: o4u44BO4Cf8QUSJRxs9eDkO3
#' > access_token = auth0$get_access_token()
#'   INFO [2020-12-01 14:03:51] access_token: eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXi5sNKR6t7iXICoBYg3N5GhM4CaP0uk_A
#'
#'
#' @export
Auth0DeviceAuth <- R6::R6Class(
  "Auth0DeviceAuth",
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  #' @description
  #' private fields
  private = list(
    .tenant_url = NULL,
    .client_id = NULL,
    .scopes = NULL,
    .audience = NULL
  ),

  #' @description
  #' public fields
  public = list(
    #' @field verification_uri character, verification uri
    verification_uri = NULL,
    #' @field verification_uri_complete character, verification uri with code
    verification_uri_complete = NULL,
    #' @field user_code character, user code
    user_code = NULL,
    #' @field device_code character, device code
    device_code = NULL,
    #' @field access_token character access_token
    access_token = NULL,
    #' @field expires_in character access_token expire
    expires_in = NULL,

    #' @description
    #' Initialization method.
    #' @param tenant_url tenant url
    #' @param client_id client_id url
    #' @param scopes scopes url
    #' @param audience audience url
    initialize = function(
      tenant_url = NULL,
      client_id = NULL,
      scopes = NULL,
      audience= NULL
    ) {
      # if no argument passed, use the environment variables
      # auth0 tenant url
      if (missing(tenant_url))
        private$.tenant_url = Sys.getenv("AUTH0_TENANT_URL")
      else
        private$.tenant_url = tenant_url

      # auth0 client id
      if (missing(client_id))
        private$.client_id = Sys.getenv("AUTH0_CLIENT_ID")
      else
        private$.client_id = client_id

      # auth0 scopes
      if (missing(scopes))
        private$.scopes = Sys.getenv("AUTH0_SCOPES")
      else
        private$.scopes = scopes

      # auth0 audience
      if (missing(audience))
        private$.audience = Sys.getenv("AUTH0_AUDIENCE")
      else
        private$.audience = tenant_url

      # set access_token in environment variable if exists for testing purpose
      self$access_token = Sys.getenv("TEST_ACCESS_TOKEN")
    },

    #' @description
    #' get_device_code get device code
    #'
    #' @return Response \code{httr::Response}
    #' https://httr.r-lib.org
    get_device_code = function() {
      url = paste0('https://', private$.tenant_url, '/oauth/device/code')
      response = POST(url,
                      body = list(client_id = private$.client_id,
                                  scopes    = private$.scopes,
                                  audience  = private$.audience),
                      encode = "form")
      content = content(response)
      self$verification_uri = content[["verification_uri"]]
      self$verification_uri_complete = content[["verification_uri_complete"]]
      self$user_code = content[["user_code"]]
      self$device_code = content[["device_code"]]

      log_info(paste("verification_uri_complete", self$verification_uri_complete, sep = ": "))
      log_debug(paste("verification_uri", self$verification_uri, sep = ": "))
      log_debug(paste("user_code", self$user_code, sep = ": "))
      log_debug(paste("device_code", self$device_code, sep = ": "))
      content
    },

    #' @description
    #' get_access_token
    #'
    #' @return character access code
    get_access_token = function() {
      if ( is.null(self$device_code) )
        stop("Unauthorized, please run get_device_code to login first")

      url = paste0('https://', private$.tenant_url, '/oauth/token')
      response = POST(url,
                      body = list(grant_type  = 'urn:ietf:params:oauth:grant-type:device_code',
                                  device_code = self$device_code,
                                  client_id   = private$.client_id),
                      encode = "form")
      content = content(response)
      self$access_token  = content[["access_token"]]
      self$expires_in  = content[["expires_in"]]
      log_debug(paste("access_token", self$access_token, sep = ": "))
      log_info(paste("access_token length", str_length(self$access_token), sep = ": "))
      self$access_token
    },

    #' @description
    #' finalize
    finalize = function() {
      log_info("finalize", self$path)
    },

    #' @description
    #' print
    print = function() {
      cat("Auth0DeviceAuth: \n")
      cat("  url_base: ", self$url_base, "\n", sep="")
      cat("  .token: ", private$.token, "\n", sep="")
      invisible(self)
    }
  ),

  lock_objects = FALSE,
  lock_class = TRUE
)


