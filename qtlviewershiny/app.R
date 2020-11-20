library(shiny)
library(httr)

ui <- htmlTemplate( text_ =
    includeHTML("www/index.html")
    )

server <- function(input, output, session) {
    # app <- get_restrserve_application()
    #
    # request <- create_new_request('GET', '/datasets')
    # response <- app$process_request(request)
    # cat(paste("Status: ", response$status))
    #

    observeEvent(input$js_action,{
        action <- fromJSON(input$js_action)
        logger$debug( paste(action$id, action$URL, collapse = "\n"))
        app <- get_restrserve_application()
        parsed_url <- parse_url(action$URL)

        request <- RestRserve::Request$new()
        request$method <- 'GET'
        request$path <- parsed_url$path
        request$parameters_query = parsed_url$query

        response <- app$process_request(request)
        action[["data"]] <- response$body
        session$sendCustomMessage("js_action_handler",  toJSON(action))
#        session$sendCustomMessage( paste("js_action_handler_", action$id), input$js_action)
    })
}

# shinyApp(ui, server)

