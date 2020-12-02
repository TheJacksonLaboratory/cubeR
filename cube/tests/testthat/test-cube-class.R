context("test Cube class")
library(here)

# test that requires sourcing the files under test
pkg_path <- here("R")
for (f in list.files(pkg_path)) {
  source(file.path(pkg_path, f))
}

cube_obj <- Cube$new()

test_that("test get_metadata ", {
  response <- cube_obj$get_metadata(element_id = 122, page_size = 5)
  expect_equal(200, response$status_code)
})


test_that("test get_metadata_pivot ", {
  response <- cube_obj$get_metadata(element_id = 122, page_size = 2)
  expect_equal(200, response$status_code)
})

test_that("test accession id filter ", {
  response <- cube_obj$get_metadata(element_id = 122, page_size = 10,
            accession_ids = "JAXAS0005k")
  expect_equal(200, response$status_code)
})

