library(shiny)
library(shinyjs)

moduleTestUI <- function(id){

  ns <- NS(id)

  tagList(
    h1("Tests to ensure shinyjs works in Shiny modules"),
    br(),
    h4("Clicking on the Name text input will show an alert box after 1 second"),
    div(id = ns('form'),
        textInput(ns('text'), 'Name', value = "Dean"),
        # colourInput(ns('col'), 'Colour', value = "blue"),

        actionButton(ns("resetForm"), "Reset form"),
        actionButton(ns("resetText"), "Reset text"),
        actionButton(ns("hideForm"), "Hide form"),
        actionButton(ns("toggleText"), "Toggle text"),
        actionButton(ns("disableText"), "Disable text")
    )
  )
}


moduleTest <- function(input, output, session) {
  onclick("text", delay(1000, info("clicked me!")))
  onevent("mouseenter", "text", logjs("entered"))

  observeEvent(input$resetForm, reset("form"))
  observeEvent(input$resetText, reset("text"))
  observeEvent(input$hideForm, hide("form"))
  observeEvent(input$toggleText, toggle("text", anim = TRUE))
  observeEvent(input$disableText, disable("text"))
}


ui <- fluidPage(
  useShinyjs(debug = TRUE),
  moduleTestUI('test')
)
server <- function(input, output, session) {
  callModule(moduleTest, 'test')
}
shinyApp(ui = ui, server = server)
