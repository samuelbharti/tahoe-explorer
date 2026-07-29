# App server. Each registered page mounts its own module server(s).
function(input, output, session) {
  mount_page_servers()

  # Let the assistant know the active page and drive tab navigation (see
  # R/agent_bridge.R). Both live in session$userData, shared with the chat
  # module's session. The page filter bridges register themselves per page.
  session$userData[[.tahoe_nav_key]] <- function(page) {
    bslib::nav_select("main_nav", page, session = session)
  }
  observeEvent(
    input$main_nav,
    session$userData[[.tahoe_active_page_key]] <- input$main_nav,
    ignoreNULL = FALSE
  )

  # App-wide assistant sidebar (see ui.R), driven by the shared chat server. It
  # starts closed; the navbar "Assistant" button toggles it open/closed so it
  # never blocks the workflow.
  chat_agent_server("chat_dock", active_page = reactive(input$main_nav))
  observeEvent(input$toggle_assistant, {
    bslib::sidebar_toggle("assistant_dock", session = session)
  })

  # The assistant (600px) and a page's filter sidebar are too much side by side,
  # so couple them: opening the assistant closes every page's filter sidebar,
  # and closing it reopens them. ignoreInit keeps the natural defaults on load
  # (assistant closed, filters open). input$assistant_dock is the sidebar's
  # collapse state (TRUE = open).
  .filter_sidebars <- c(
    "drugs-filters_sidebar",
    "cell_lines-filters_sidebar",
    "coverage-filters_sidebar",
    "subset-filters_sidebar",
    "obs-filters_sidebar",
    "obs-query_sidebar"
  )
  observeEvent(input$assistant_dock, ignoreInit = TRUE, {
    open_filters <- !isTRUE(input$assistant_dock)
    for (sb in .filter_sidebars) {
      bslib::sidebar_toggle(sb, open = open_filters, session = session)
    }
  })

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
