#' These are the util function shared by the package
#'


#' @description
#' to_json convert a HTTP json response \code{httr::Response} to R
#'    data \code{jsonlite::fromJSON}.
#' @param response \code{httr::Response} http response
#'
#' @return data \code{jsonlite::fromJSON}. The data could be list or data frame
#'   depend on the json
#'   https://httr.r-lib.org
response_json_to_data = function(
  response
) {
  content = content(response, "text")
  data = fromJSON(content)
}
