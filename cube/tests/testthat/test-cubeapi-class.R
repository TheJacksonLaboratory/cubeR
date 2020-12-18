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
  response = cube_api$get_disclaimer()
  json = response_json_to_data(response)
  log_info(paste("count", json$count))

  expect_equal(json$count, 1)
})

test_that("test get_element with id path parameter ", {
  response = cube_api$get_element(element_id = 122)
  json = response_json_to_data(response)
  expect_equal(json$id, 122)
})

test_that("test get element instance ", {
  response <- cube_api$get_element_instance()

  expect_equal(response$status_code, 200)
})

test_that("test get element instance with parameters ", {
  response <- cube_api$get_element_instance(element_id = 122,
                                            page_size = 2)

  expect_equal(response$status_code, 200)
})

test_that("test post_element_instance_filter ", {
  response = cube_api$post_element_instance_filter(element_id = 122,
                                                   page = 1,
                                                   page_size = 2)
  expect_equal(response$status_code, 200)
  #json = response_json_to_data(response)
  #expect_equal(json$count, 2)
})

test_that("test get_metadata_collection_storage_info ", {
  df <- cube_api$get_metadata_collection_storage_info()

  expect_equal(ncol(df), 4)
})


