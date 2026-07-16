# Chat assistant page. Self-registers into the navbar (see R/app_pages.R). The UI
# is evaluated at source time, so chat_agent_ui() must be safe when the assistant
# is unavailable -- it is (it renders a bslib-only setup panel). Placed after the
# Subset builder (order 40) since its main job is helping plan subsets.
register_page(
  id = "chat",
  title = "Chat",
  order = 45,
  ui = chat_agent_ui("chat"),
  server = function() chat_agent_server("chat"),
  # Fillable so the chat fills the viewport (only the message list scrolls); the
  # UI wraps itself in a fill carrier to match. See modules/chat_mod.R.
  fillable = TRUE
)
