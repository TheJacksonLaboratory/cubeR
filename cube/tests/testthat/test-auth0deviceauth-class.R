context("test Auth0DeviceAuth class")
library(here)
library(httr)

# test that requires sourcing the files under test
pkg_path <- here("R")
for (f in list.files(pkg_path)) {
  source(file.path(pkg_path, f))
}

auth0 = Auth0DeviceAuth$new()

test_that("test get_device_code, get_access_token", {
  content = auth0$get_device_code()
  verification_uri = content[["verification_uri"]]
  device_code = content[["device_code"]]
  user_code = content[["user_code"]]

  log_debug(paste("verification_uri", verification_uri, sep = ": "))
  log_debug(paste("device_code", device_code, sep = ": "))
  log_debug(paste("user_code", user_code, sep = ": "))

  access_token = auth0$get_access_token()
  log_debug(paste("access_token", access_token, sep = ": "))

  expect_equal(9, str_length(user_code))

})



