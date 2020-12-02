context("test CubeAPI class")
library(here)
library(httr)
library(jsonlite)

# test that requires sourcing the files under test
pkg_path <- here("R")
for (f in list.files(pkg_path)) {
  source(file.path(pkg_path, f))
}

cube_api <- CubeAPI$new()
#cube_api$login()

test_that("test get disclaimer", {
  response <- cube_api$get_disclaimer()
  log_info(response$status_code)

  expect_equal(response$status_code, 200)
})

test_that("test get disclaimer json ", {
  json <- cube_api$get_disclaimer_json()
  log_info(paste("count", json$count))

  expect_equal(json$count, 1)
})


test_that("test get element instance ", {
  response <- cube_api$get_element_instance(page=1, page_size = 100)

  expect_equal(response$status_code, 200)
  log_info(response$status_code)
  if ( response$status_code != 200 ) {
    log_error( paste0("Error: ", response ) )
    stop()
  }
  json_data <- fromJSON(content(response, "text"))
  # hack
  expect_equal(3945 , json_data$count)
})

