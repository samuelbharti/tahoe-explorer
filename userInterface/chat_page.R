# Chat assistant page. Self-registers into the navbar (see R/app_pages.R). The UI
# is evaluated at source time, so chat_agent_ui() must be safe when the assistant
# is unavailable -- it is (it renders a bslib-only setup panel). Placed after the
# Subset builder (order 40) since its main job is helping plan subsets.
register_page(
  id = "chat",
  title = "Chat",
  order = 45,
  ui = chat_agent_ui("chat"),
  server = function() chat_agent_server("chat")
  # Not a fillable panel: a fillable panel forces the whole page to a fixed
  # viewport height, which pins the app footer mid-page over tall tabs. The chat
  # instead gets an explicit viewport-relative height in modules/chat_mod.R.
)
