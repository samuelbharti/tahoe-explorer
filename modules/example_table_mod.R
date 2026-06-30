example_table_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Example Table Module"),
    tableOutput(ns("preview"))
  )
}

example_table_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    output$preview <- renderTable({
      req(data_reactive())
      head(data_reactive(), 10)
    })
  })
}
