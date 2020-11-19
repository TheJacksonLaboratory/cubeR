context("test Database class")
library(here)

# test that requires sourcing the files under test
pkg_path <- here("R")
for (f in list.files(pkg_path)) {
  source(file.path(pkg_path, f))
}

db_obj <- Database$new()

test_that("test number of row returned", {
  query <- "SELECT * from public.metadata_definition_element limit 3"
  results <- db_obj$select(query);

  expect_equal(3, nrow(results))
})


test_that("test get_column_info", {

  results <- db_obj$get_column_info(85);

  expect_equal(2, ncol(results))
})
