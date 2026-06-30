example_counter_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Example Module"),
    p("Use this module as a starter pattern for UI + server separation."),
    actionButton(ns("increment"), "Increment"),
    br(),
    br(),
    textOutput(ns("value"))
  )
}

example_counter_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    counter <- reactiveVal(0)

    observeEvent(input$increment, {
      counter(counter() + 1)
    })

    output$value <- renderText({
      paste("Current value:", counter())
    })
  })
}
