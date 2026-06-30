example_plot_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Example Plot Module"),
    plotOutput(ns("plot"), height = 260)
  )
}

example_plot_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot({
      req(data_reactive())
      dat <- data_reactive()
      validate(need(ncol(dat) >= 2, "Need at least two columns for plotting"))

      x <- dat[[1]]
      y <- dat[[2]]
      plot(x, y, pch = 16, col = "#2c7fb8", main = "Example Scatter Plot")
    })
  })
}
