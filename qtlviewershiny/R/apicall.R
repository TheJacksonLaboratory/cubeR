library(RestRserve)

create_new_request <- function(method, path, parameters_query = NULL) {
    request <- RestRserve::Request$new()
    request$method <- method
    request$path <- path
    request$parameters_query <- parameters_query
    return (request)
}

test <- function() {
    app <- get_restrserve_application()

    request <- create_new_request('GET', '/datasets')
    response <- app$process_request(request)
    response
}
