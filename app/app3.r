library(shiny)

ui <- function(req) {
  # The `req` object is a Rook environment
  # See https://github.com/jeffreyhorner/Rook#the-environment
  if (identical(req$REQUEST_METHOD, "GET")) {
    # 
    fluidPage(
      # as normal...
    )
  } else if (identical(req$REQUEST_METHOD, "POST")) {
    # Handle the POST
    query_params <- parseQueryString(req$QUERY_STRING)
    body_bytes <- req$rook.input$read(-1)

    # Be sure to return a response
    httpResponse(
      status = 200L,
      content_type = "application/json",
      content = '{"status": "ok"}'
    )
  }
}
attr(ui, "http_methods_supported") <- c("GET", "POST")

server <- function(input, output, session) {
  # same as usual
}

shinyApp(ui, server)