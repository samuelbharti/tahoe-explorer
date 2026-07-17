# App server. Each registered page mounts its own module server(s).
function(input, output, session) {
  mount_page_servers()

  # Guided demo. The navbar "Demo" button jumps to the Drugs tab and starts the
  # click-through tour. The guide is initialised once per session. The start is
  # deferred briefly so the Drugs tab-pane is laid out before cicerone measures
  # its first target (starting in the same flush lands on a hidden element).
  guide <- drug_tour()
  guide$init()
  observeEvent(input$demo_tour, {
    bslib::nav_select("main_nav", "drugs", session = session)
    later::later(function() guide$start(session = session), delay = 0.4)
  })
}
