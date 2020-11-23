context("test Cube class")
library(here)

# test that requires sourcing the files under test
pkg_path <- here("R")
for (f in list.files(pkg_path)) {
  source(file.path(pkg_path, f))
}

cube_obj <- Cube$new(dataset_id = 85, accession_ids = c("JAXAS00001", "JAXAS00002") )

test_that("test get_metadata ", {
  metadata <- cube_obj$get_metadata(dataset_id = 85, max_number_of_row = 5)
  expect_equal(5, nrow(metadata))
})


test_that("test get_metadata_pivot ", {
  metadata <- cube_obj$get_metadata(dataset_id = 85, max_number_of_row = 2,
                                    is_pivot = TRUE)
  expect_equal(2, nrow(metadata))
})

test_that("test accession id filter ", {
  metadata <- cube_obj$get_metadata(dataset_id = 85, max_number_of_row = 10,
            accession_ids = c("JAXAS00004", "JAXAS00006"))
  expect_equal(10, nrow(metadata))
})

