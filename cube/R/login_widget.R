library(htmlwidgets)

#' login widget
#'
#' This is login widget for Cube API login.  In RStudio, it will show in viewer
#' and in R notebook, it shows as cell
#'
#' @import htmlwidgets
#'
#' @param validation_url url for validation
#'
#' @export

login_widget <- function(validation_url, width = 400, height = 100, elementId = NULL) {

  # forward options using x
  x = list(
    validation_url = validation_url
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'login_widget',
    x,
    width = width,
    height = height,
    package = 'cube',
    elementId = elementId
  )
}
