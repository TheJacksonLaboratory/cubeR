context("test Cube class")
library(here)

# test that requires sourcing the files under test
pkg_path <- here("R")
for (f in list.files(pkg_path)) {
  source(file.path(pkg_path, f))
}

cube_obj <- Cube$new()

test_that("test get_metadata ", {
  metadata <- cube_obj$get_metadata(element_id = 122, page_size = 5)
  expect_equal(5, nrow(metadata))
})


test_that("test get_metadata_pivot ", {
  metadata <- cube_obj$get_metadata(element_id = 122, page_size = 2,
                                    is_pivot = TRUE)
  expect_equal(2, nrow(metadata))
})

test_that("test accession id filter ", {
  metadata <- cube_obj$get_metadata(element_id = 122, page_size = 10,
            accession_ids = c("JAXAS0005k", "JAXAS0005J"))
  expect_equal(10, nrow(metadata))
})

