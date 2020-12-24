# install required packages

pkgs <- c(
  "cloudml",
  "jsonlite",
  "httr",
  "htmlwidgets",
  "logger",
  "rapportools",
  "R6",
  "stringr",
  "testthat",
  "here")

install.packages(pkgs = pkgs,
                 repos = "http://cran.us.r-project.org"
)

options(stringsAsFactors = FALSE)
