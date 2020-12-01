library(RPostgreSQL)

#' Database
#'
#' Class \code{database} defines a database object.
#'
#' @name Database-class
#' @rdname Database-class
#' @exportClass Database
#'
#' @slot host character, database host
#'   parameters
#'
#' @export
Database <- R6::R6Class(
  "Database",
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  private = list(
    .conn = NULL
  ),
  public = list(
    #' @field host database host
    host = NULL,
    #' @field port database port
    port = 5432,
    #' @field dbname database name
    dbname = NULL,
    #' @field user database user id
    user = NULL,
    #' @field password user password
    password = NULL,

    #' @description
    #' Initialization method.
    #' @param host database host
    #'  required
    #' @param port database port
    #'  required
    #' @param dbname database name
    #'  required
    #' @param user database user id
    #'  required
    #' @param password database user password
    #'  required

    initialize = function(
      host = NULL,
      port = NULL,
      dbname = NULL,
      user = NULL,
      password = NULL
    ) {
      if (missing(host)) self$host = Sys.getenv("DB_HOST")
      if (missing(port)) self$port = Sys.getenv("DB_PORT")
      if (missing(dbname)) self$dbname = Sys.getenv("DB_NAME")
      if (missing(user)) self$user = Sys.getenv("DB_USER")
      if (missing(password)) self$password = Sys.getenv("DB_PASS")

      private$.conn = RPostgreSQL::dbConnect(
                        drv = RPostgreSQL::PostgreSQL(),
                        host=self$host,
                        port=self$port,
                        dbname=self$dbname,
                        user=self$user,
                        password=self$password)
    },

    #' @description
    #' select method.
    #' @param query a SQL query string
    #'  required
    #'
    #' @return data.frame query_results
    select = function(query) {
      query_results = RPostgreSQL::dbGetQuery(private$.conn, query)
      query_results
    },

    #' @description
    #' test method.
    #' This is a test function database connection and query
    #' @return data.frame query_results
    test = function() {
      query = "SELECT * from public.metadata_definition_element limit 5"
      results = self$select(query);
      results
    },

    #' @description
    #' get_column_info method.
    #' This is a function to get metadata column names
    #' @param element_id integer, element id
    #'
    #' @return data.frame of two columns, column name and data type
    get_column_info = function(element_id) {
      query = paste("SELECT property_label, data_type
          FROM metadata_definition_property p
          WHERE element_id=", element_id,
          "AND include_in_summary = 'true'",
          "ORDER BY id", sep = " ")
      results = self$select(query)
      results  # return as a vector
    },

    #' @description
    #' finalize
    finalize = function() {
      message("Close database connection ", self$path)
      RPostgreSQL::dbDisconnect(private$.conn)
      unlink(self$path)
    },

    #' @description
    #' print
    print = function() {
      cat("Database: \n")
      cat("  host: ", self$host, "\n", sep="")
      cat("  port: ", self$port, "\n", sep="")
      cat("  dbname: ", self$dbname, "\n", sep="")
      cat("  user: ", self$user, "\n", sep="")
      cat("  password: ", self$password, "\n", sep="")
      invisible(self)
    }
  ),

  lock_objects = FALSE,
  lock_class = TRUE
)






