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

      log_info(paste("verification_uri", self$verification_uri, sep = ": "))
      log_info(paste("verification_uri_complete", self$verification_uri_complete, sep = ": "))
      log_info(paste("user_code", self$user_code, sep = ": "))
      log_info(paste("device_code", self$device_code, sep = ": "))
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
      log_info(paste("access_token", self$access_token, sep = ": "))
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


