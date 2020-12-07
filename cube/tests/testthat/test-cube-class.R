context("test Cube class")
library(here)

# test that requires sourcing the files under test
pkg_path <- here("R")
for (f in list.files(pkg_path)) {
  source(file.path(pkg_path, f))
}

cube_obj <- Cube$new()

test_that("test get all element ", {
  response <- cube_obj$get_element()
  expect_equal(200, response$status_code)
})

test_that("test get element with element_id parameter ", {
  response <- cube_obj$get_element(element_id = 122)
  json = response_json_to_data(response)
  expect_equal(json$id, 122)
})

test_that("test get metadata", {
  response <- cube_obj$get_metadata()
  expect_equal(200, response$status_code)
})


test_that("test get metadata accession id filter ", {
  response <- cube_obj$get_metadata(element_id = 83,
                                    accession_ids = "JAXIM00063")
  expect_equal(200, response$status_code)
  json = response_json_to_data(response)
  expect_equal(json$count, 1)
})

test_that("test multiple accession ids filter ", {
  response <- cube_obj$get_metadata(element_id = 83,
                                    accession_ids = c("JAXIM00063", "JAXIM00064"))
  expect_equal(200, response$status_code)
  json = response_json_to_data(response)
  expect_equal(json$count, 2)
})
