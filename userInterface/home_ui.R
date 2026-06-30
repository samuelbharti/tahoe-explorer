home_page <- fluidPage(
  titlePanel("Home"),
  hr(),
  fluidRow(
    column(
      width = 8,
      p("Start here: customize this page with your app introduction."),
      example_counter_ui("home_counter")
    ),
    column(
      width = 4,
      tags$h4("Quick Notes"),
      tags$ul(
        tags$li("Place shared controls in modules/"),
        tags$li("Keep page layouts in userInterface/"),
        tags$li("Keep heavy compute out of render blocks")
      )
    )
  )
)
