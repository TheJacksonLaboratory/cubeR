library(jsonlite)

#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export

qtlviewer <- function(title, data_api_url = NULL, width = NULL, height = NULL, elementId = NULL) {

  file <- system.file("widget.html", package = "qtlviewerwidget")

  widget_html = paste(readLines(file, warn=FALSE, skipNul=TRUE), collapse="\n")

#  chromosomes <- jsonlite::read_json(path ="data-raw/chromosomes.json")
#  correlation <- jsonlite::read_json(path ="data-raw/correlation.json")
#  dataset_exvivo <- jsonlite::read_json(path ="data-raw/dataset.exvivo.json")
#  datasets <- jsonlite::read_json(path ="data-raw/datasets.json")
#  expression <- jsonlite::read_json(path ="data-raw/expression.json")
#  lod <- jsonlite::read_json(path ="data-raw/lod.json")


  # forward options using x
  x = list(
    title = title,
    data_api_url = data_api_url,
    widget_html = widget_html
#    chromosomes = chromosomes,
#    correlation = correlation,
#    dataset_exvivo = dataset_exvivo,
#    datasets = datasets,
#    expression = expression,
#    lod = lod
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'qtlviewer',
    x,
    width = width,
    height = height,
    package = 'qtlviewer',
    elementId = elementId
  )
}

#' Shiny bindings for qtlviewer
#'
#' Output and render functions for using qtlviewer within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a qtlviewer
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name qtlviewer-shiny
#'
#' @export
qtlviewerOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'qtlviewer', width, height, package = 'qtlviewer')
}

#' @rdname qtlviewer-shiny
#' @export
renderQtlviewer <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, qtlviewerOutput, env, quoted = TRUE)
}
