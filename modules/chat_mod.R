# Chat assistant module.
#
# A shinychat chat UI backed by a per-session ellmer Chat (Gemini on Vertex) with
# hand-written tools over the app's data layer (see R/agent.R, R/agent_tools.R).
#
# Opt-in and gracefully degrading: when the assistant is not available (packages
# missing, no Vertex project configured, or force-disabled) the page renders a
# setup panel built purely from bslib -- it references NO ellmer/shinychat symbols
# -- so the app (and the CI smoke test) load fine without those packages.

# Setup panel shown when the assistant is off. Uses only bslib/shiny, so it is
# safe to evaluate at UI-build time even when ellmer/shinychat are not installed.
.chat_disabled_panel <- function() {
  bslib::card(
    bslib::card_header("Configure Google Vertex to enable the assistant"),
    bslib::card_body(
      p("The AI assistant is off in this environment. To enable it:"),
      tags$ol(
        tags$li(
          "Install the ",
          tags$code("ellmer"),
          " and ",
          tags$code("shinychat"),
          " R packages (both are in renv.lock)."
        ),
        tags$li(
          "Authenticate with Google Cloud: ",
          tags$code("gcloud auth application-default login"),
          "."
        ),
        tags$li(
          "Set ",
          tags$code("TAHOE_VERTEX_PROJECT"),
          " (and optionally ",
          tags$code("TAHOE_VERTEX_LOCATION"),
          ") in your ",
          tags$code(".Renviron"),
          " -- see ",
          tags$code(".Renviron.example"),
          "."
        )
      ),
      p(
        class = "text-muted small",
        "The rest of Tahoe Explorer works normally without the assistant."
      )
    )
  )
}

chat_agent_ui <- function(id) {
  ns <- NS(id)
  header <- div(
    class = "p-2",
    h3("Ask the Tahoe assistant"),
    p(
      class = "text-muted",
      "A Gemini-backed assistant with tools over this app's data layer. It can",
      "explain the Tahoe-100M dataset and this app, and help you plan a subset."
    )
  )

  if (!tahoe_agent_available()) {
    return(tagList(header, .chat_disabled_panel()))
  }

  tagList(
    header,
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "Session",
        width = 260,
        uiOutput(ns("usage")),
        tags$p(
          class = "text-muted small",
          "One assistant per session. Ask about Tahoe-100M, the app, drugs,",
          "cell lines, or building a subset."
        )
      ),
      shinychat::chat_ui(
        ns("chat"),
        height = "70vh",
        placeholder = "Ask about the dataset, drugs, cell lines, or a subset...",
        greeting = paste(
          "Hi! I can explain the **Tahoe-100M** dataset and this app, and help",
          "you plan a subset for your analysis. What are you working on?"
        )
      )
    )
  )
}

#' Chat assistant server. `client_factory` builds the ellmer Chat (defaults to
#' tahoe_agent_client); `append` sends a response to the chat widget (defaults to
#' shinychat::chat_append). Both are injectable so tests can drive the module with
#' a stub client and a recorder -- no network, no credentials. Injecting a
#' `client_factory` forces the enabled path even when the assistant is otherwise
#' unavailable.
chat_agent_server <- function(id, client_factory = NULL, append = NULL) {
  moduleServer(id, function(input, output, session) {
    enabled <- !is.null(client_factory) || tahoe_agent_available()
    if (!enabled) {
      return(invisible(NULL))
    }

    factory <- if (is.null(client_factory)) {
      tahoe_agent_client
    } else {
      client_factory
    }
    do_append <- if (is.null(append)) {
      # Pass the PLAIN id "chat" -- inside moduleServer shinychat namespaces
      # against the module session; session$ns("chat") would double-namespace.
      function(response) shinychat::chat_append("chat", response)
    } else {
      append
    }

    client <- tryCatch(
      factory(),
      error = function(e) {
        warning("chat client init failed: ", conditionMessage(e))
        NULL
      }
    )

    # Record which tools fired this turn, for a "tools used" footer.
    tools_used <- reactiveVal(character())
    if (!is.null(client) && is.function(client$on_tool_request)) {
      client$on_tool_request(function(request) {
        nm <- tryCatch(
          request@name,
          error = function(e) tryCatch(request$name, error = function(e2) NULL)
        )
        if (!is.null(nm) && nzchar(nm)) {
          isolate(tools_used(c(tools_used(), nm)))
        }
      })
    }

    n_turns <- reactiveVal(0L)

    observeEvent(input$chat_user_input, {
      msg <- input$chat_user_input
      if (is.null(msg) || !nzchar(trimws(msg))) {
        return()
      }
      if (is.null(client)) {
        do_append(paste(
          "The assistant failed to initialize.",
          "Check the server logs and your Vertex credentials."
        ))
        return()
      }
      if (n_turns() >= .tahoe_agent_max_turns()) {
        do_append(paste(
          "_Turn limit reached for this session._",
          "_Reload the page to start a new conversation._"
        ))
        return()
      }
      n_turns(n_turns() + 1L)
      tools_used(character())

      stream <- client$stream_async(msg)
      p <- do_append(stream)

      if (!is.null(p) && inherits(p, "promise")) {
        promises::then(
          p,
          onFulfilled = function(value) {
            used <- unique(tools_used())
            if (length(used) > 0) {
              do_append(paste0(
                "*tools used: ",
                paste(used, collapse = ", "),
                "*"
              ))
            }
          },
          onRejected = function(err) {
            do_append(paste0(
              "Sorry, something went wrong: ",
              conditionMessage(err)
            ))
          }
        )
      }
    })

    output$usage <- renderUI({
      div(
        class = "text-muted small",
        sprintf("Turns this session: %d", n_turns())
      )
    })

    invisible(list(
      client = client,
      n_turns = n_turns,
      tools_used = tools_used
    ))
  })
}
