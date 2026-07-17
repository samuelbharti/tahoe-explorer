# App server. Each registered page mounts its own module server(s).
function(input, output, session) {
  mount_page_servers()

  # Guided demo. Every page has a cicerone tour (R/tour.R); the navbar "Demo"
  # button starts the tour for whichever tab is active. Guides are built and
  # initialised once per session. The start is deferred briefly so the tab pane
  # is laid out before cicerone measures its first target (a same-flush start
  # can land on an element that has not finished showing).
  guides <- lapply(tahoe_tours(), function(build) build())
  for (g in guides) {
    g$init()
  }
  observeEvent(input$demo_tour, {
    guide <- guides[[input$main_nav %||% ""]]
    if (is.null(guide)) {
      showNotification(
        "No demo is available for this page yet.",
        type = "message"
      )
      return()
    }
    later::later(function() guide$start(session = session), delay = 0.3)
  })
}
