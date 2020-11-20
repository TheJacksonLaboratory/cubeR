context("test Rest Server")
library(here)
library(httr)

# test that requires sourcing the files under test
pkg_path <- here("R")
for (f in list.files(pkg_path)) {
  source(file.path(pkg_path, f))
}

app <- get_restrserve_application()

test_that("test get_datasets ", {
  request <- create_new_request('GET', '/datasets')
  response <- app$process_request(request)

  expect_equal(response$status_code, 200)
})


test_that("test lodpeaksall ", {
    URL = "/lodpeaksall?dataset=dataset.DOheart.mrna"
    URL = "/correlation?dataset=dataset.exvivo&id=G33_ins_secrete_gm"
    URL = "/lodscan?dataset=dataset.exvivo&id=G33_ins_secrete_gm&intcovar=additive&cores=5"
    URL = "/lodscan?dataset=dataset.DOheart.mrna&id=ENSMUSG00000038235&intcovar=additive&cores=5"
    URL = "/dataset=dataset.DOheart.mrna&id=ENSMUSG00000038235"
    URL = "/lodscan?dataset=dataset.liver.nopoly.proteins&id=ENSMUSP00000041907&intcovar=additive"
    URL = "/foundercoefs?dataset=dataset.liver.nopoly.proteins&id=ENSMUSP00000041907&chrom=1&intcovar=additive&blup=false&cores=5"
    URL = "/lodscan?dataset=dataset.exvivo&id=G33_ins_secrete_gm&intcovar=additive&cores=5"
    parsed_url <- parse_url(URL)

    request <- RestRserve::Request$new()
    request$method <- 'GET'
    request$path <- parsed_url$path
    request$parameters_query = parsed_url$query

    message(paste(request$method, request$path, paste(request$query)))

    response <- app$process_request(request)

    expect_equal(response$status_code, 200)
})

