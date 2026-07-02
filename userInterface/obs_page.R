# Samples & cell-level obs explorer tab.

register_page(
  id = "obs",
  title = "Samples & cells",
  order = 30,
  ui = obs_explorer_ui("obs"),
  server = function() obs_explorer_server("obs")
)
