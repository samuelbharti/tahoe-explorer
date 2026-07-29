# Chat assistant module.
#
# A shinychat chat UI backed by a per-session ellmer Chat with hand-written tools
# over the app's data layer (see R/agent.R, R/agent_tools.R). Two credential
# paths, chosen live via a "Model source" selector:
#   * the shared server DEFAULT (Gemini on Vertex, the operator's bill), and
#   * BYOK -- the user pastes their own Gemini/OpenAI/Anthropic key (their bill),
#     held only in this session's memory.
#
# Opt-in and gracefully degrading: when neither path is available (packages
# missing or force-disabled) the page renders a setup panel built purely from
# bslib -- it references NO ellmer/shinychat symbols -- so the app (and the CI
# smoke test) load fine without those packages.

# Setup panel shown when the assistant is off. Uses only bslib/shiny, so it is
# safe to evaluate at UI-build time even when ellmer/shinychat are not installed.
.chat_disabled_panel <- function() {
  bslib::card(
    bslib::card_header("Enable the AI assistant"),
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
          "Then either configure the shared assistant (Google Vertex -- run ",
          tags$code("gcloud auth application-default login"),
          " and set ",
          tags$code("TAHOE_VERTEX_PROJECT"),
          " in ",
          tags$code(".Renviron"),
          "), or simply bring your own API key in the chat sidebar."
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
      "An AI assistant with tools over this app's data layer. It can explain the",
      "Tahoe-100M dataset and this app, and help you plan a subset."
    )
  )

  if (!tahoe_agent_enabled()) {
    return(tagList(header, .chat_disabled_panel()))
  }

  # Model-source choices: the shared default (when configured) plus every offered
  # BYOK provider. Names are user-facing labels; values are stable ids.
  choices <- character(0)
  if (tahoe_agent_default_available()) {
    choices <- c(choices, "Shared assistant (Gemini on Vertex)" = "shared")
  }
  for (p in tahoe_agent_byok_providers()) {
    choices[.tahoe_agent_provider_meta(p)$label] <- p
  }
  if (length(choices) == 0) {
    return(tagList(header, .chat_disabled_panel()))
  }

  # Prefer a source that connects without pasting: the shared Vertex default if
  # configured, otherwise a BYOK provider whose key is already in the
  # environment, otherwise the first offered provider.
  default_source <- if (tahoe_agent_default_available()) {
    "shared"
  } else {
    tahoe_agent_env_provider() %||% unname(choices)[1]
  }

  # Fill carrier so the sidebar+chat layout grows to fill the (fillable) Chat
  # panel: the chat fills the viewport minus the header and only the message list
  # scrolls, so the page itself never scrolls. See userInterface/chat_page.R
  # (fillable = TRUE). The compact CSS trims bslib's generous sidebar padding.
  # A div (not a tagList) so as_fill_carrier can set the fill role on a real tag.
  bslib::as_fill_carrier(div(
    class = "tahoe-chat-page",
    tags$style(HTML(
      paste0(
        # bslib's fillable-panel layout resets this pane's padding to 0, so the
        # global 4vw page gutter (ui.R) doesn't reach the Chat tab -- restore it
        # here on the page wrapper so the chat lines up with the other tabs.
        ".tahoe-chat-page",
        "{padding-left:4vw !important;padding-right:4vw !important;}",
        ".tahoe-chat-layout .sidebar-content",
        "{padding-top:12px !important;gap:8px !important;}",
        ".tahoe-chat-layout .form-group,",
        ".tahoe-chat-layout .shiny-input-container{margin-bottom:0 !important;}",
        ".tahoe-chat-layout .control-label{margin-bottom:2px !important;}"
      )
    )),
    header,
    bslib::as_fill_carrier(div(
      class = "tahoe-chat-layout",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          title = "Model source",
          width = 420,
          selectInput(
            ns("source"),
            label = NULL,
            choices = choices,
            selected = default_source
          ),
          # Key form: shown for any BYOK provider, hidden for the shared default.
          conditionalPanel(
            condition = "input.source && input.source != 'shared'",
            ns = ns,
            uiOutput(ns("key_help")),
            passwordInput(
              ns("api_key"),
              "API key",
              placeholder = "Paste your key (kept in this session only)"
            ),
            # Model picker: choose a suggested model or type any id the key
            # supports (create = TRUE). Left empty -> the provider's own default.
            # Choices are repopulated per provider in the server; the button
            # below can replace them with the key's live, current model list.
            selectizeInput(
              ns("model"),
              "Model",
              choices = character(0),
              selected = character(0),
              multiple = FALSE,
              options = list(
                create = TRUE,
                placeholder = "Provider default - pick or type a model"
              )
            ),
            tags$p(
              class = "text-muted small mb-1",
              "Not listed? Type any model id your key supports and press Enter."
            ),
            # Pull the CURRENT, key-scoped model list from the provider so the
            # picker never offers a stale or inaccessible model.
            actionButton(
              ns("refresh_models"),
              "List models for this key",
              class = "btn-outline-secondary btn-sm mb-1"
            ),
            uiOutput(ns("model_source")),
            div(
              class = "d-flex gap-2 mb-2",
              actionButton(
                ns("connect"),
                "Connect",
                class = "btn-primary btn-sm"
              ),
              actionButton(
                ns("forget"),
                "Forget key",
                class = "btn-outline-secondary btn-sm"
              )
            )
          ),
          uiOutput(ns("cred_status")),
          tags$p(
            class = "text-muted small",
            "Ask about Tahoe-100M, the app, drugs, cell lines, or building a",
            "subset. Switching source starts a fresh conversation."
          )
        ),
        shinychat::chat_ui(
          ns("chat"),
          # Explicit tall height (viewport minus navbar/header/footer chrome) so
          # the chat fills most of the screen without making the whole page a
          # fixed-height fillable container (which would break the app footer).
          height = "calc(100vh - 250px)",
          placeholder = "Ask about the dataset, drugs, cell lines, or a subset...",
          greeting = paste(
            "Hi! I can explain the **Tahoe-100M** dataset and this app, and help",
            "you plan a subset for your analysis. What are you working on?"
          )
        )
      )
    ))
  ))
}

#' Chat assistant server. `client_factory(credential)` builds the ellmer Chat
#' for a credential (defaults to tahoe_agent_client); `append` sends a response
#' to the chat widget (defaults to shinychat::chat_append). Both are injectable
#' so tests drive the module with a stub client and a recorder -- no network, no
#' credentials. Injecting a `client_factory` also forces the enabled path even
#' when the assistant is otherwise unavailable (e.g. the disabled test suite).
#' `fetch_models(provider, api_key)` returns the key's live, current model ids
#' (or NULL to fall back to the curated suggestions); it defaults to the real
#' ellmer-backed fetch and is injectable so tests avoid the network.
chat_agent_server <- function(
  id,
  client_factory = NULL,
  append = NULL,
  fetch_models = NULL
) {
  moduleServer(id, function(input, output, session) {
    enabled <- !is.null(client_factory) || tahoe_agent_enabled()
    if (!enabled) {
      return(invisible(NULL))
    }

    # Live model lister -- injectable so tests avoid the network. Defaults to the
    # real ellmer-backed fetch.
    lister <- if (is.null(fetch_models)) {
      .tahoe_agent_fetch_models
    } else {
      fetch_models
    }

    # Session-aware tools that read and drive the Subset builder (found lazily via
    # session$userData; see R/agent_bridge.R). Appended to the base data-layer
    # tool suite. `session` here is the chat session -- used only to LOOK UP the
    # bridge; the builder side owns the actual input writes.
    state_tools <- tahoe_subset_state_tools(session)
    factory <- if (is.null(client_factory)) {
      function(credential) {
        tahoe_agent_client(
          credential = credential,
          tools = c(tahoe_agent_tools(), state_tools)
        )
      }
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
    do_clear <- function() {
      tryCatch(shinychat::chat_clear("chat"), error = function(e) NULL)
    }

    client <- reactiveVal(NULL)
    tools_used <- reactiveVal(character())
    n_turns <- reactiveVal(0L)
    status <- reactiveVal(list(ok = FALSE, msg = "Not connected."))
    # The active BYOK key, held only to redact it from any surfaced error.
    # "" for the shared path or before a key is connected.
    active_secret <- reactiveVal("")
    # Note shown under the "List models" button (fallback vs. live-loaded count).
    model_note <- reactiveVal(NULL)

    # Attach the per-turn tool-name recorder to a freshly built client (for the
    # "tools used" footer). Re-attached on every rebuild.
    wire_tools <- function(cl) {
      if (!is.null(cl) && is.function(cl$on_tool_request)) {
        cl$on_tool_request(function(request) {
          nm <- tryCatch(
            request@name,
            error = function(e) {
              tryCatch(request$name, error = function(e2) NULL)
            }
          )
          if (!is.null(nm) && nzchar(nm)) {
            isolate(tools_used(c(tools_used(), nm)))
          }
        })
      }
      invisible(cl)
    }

    # Build (or clear) the session client for a credential and reset the convo.
    # `secret` is redacted from any error surfaced to the user.
    set_client <- function(credential, ok_msg, secret = "") {
      cl <- tryCatch(
        factory(credential),
        error = function(e) {
          status(list(
            ok = FALSE,
            msg = paste0(
              "Could not connect: ",
              .tahoe_agent_redact(conditionMessage(e), secret)
            )
          ))
          NULL
        }
      )
      if (!is.null(cl)) {
        wire_tools(cl)
        status(list(ok = TRUE, msg = ok_msg))
      }
      client(cl)
      n_turns(0L)
      tools_used(character())
      do_clear()
    }

    # React to the model-source selector. "shared" builds the Vertex client
    # immediately; a BYOK provider waits for the user to Connect a key.
    observeEvent(input$source, {
      src <- input$source
      if (is.null(src) || !nzchar(src)) {
        return()
      }
      active_secret("")
      if (identical(src, "shared")) {
        set_client(
          tahoe_agent_default_credential(),
          "Connected: shared assistant."
        )
      } else {
        client(NULL)
        do_clear()
        # Repopulate the model picker with this provider's suggestions.
        updateSelectizeInput(
          session,
          "model",
          choices = .tahoe_agent_provider_models(src),
          selected = character(0),
          server = FALSE
        )
        model_note(NULL)
        # If a key is already in the environment, tell the user they can skip
        # pasting and just pick a model + Connect.
        if (nzchar(.tahoe_agent_provider_env_key(src))) {
          status(list(
            ok = FALSE,
            msg = paste(
              "A key was found in your environment — pick a model and click",
              "Connect (no need to paste)."
            )
          ))
        } else {
          status(list(ok = FALSE, msg = "Enter your key and click Connect."))
        }
      }
    })

    # Fetch the provider's live, key-scoped model list and repopulate the picker.
    # Keeps whatever the user already typed selected; on failure we keep the
    # curated fallback and say so, so this can never strand the user. (BYOK only;
    # the shared Vertex source has no pasteable key to scope a listing to.)
    observeEvent(input$refresh_models, {
      src <- input$source
      if (is.null(src) || identical(src, "shared") || !nzchar(src)) {
        return()
      }
      key <- trimws(input$api_key %||% "")
      # Fall back to a key already in the environment, so the live model list
      # works without pasting.
      if (!nzchar(key)) {
        key <- .tahoe_agent_provider_env_key(src)
      }
      if (!nzchar(key)) {
        model_note(list(
          ok = FALSE,
          msg = "Enter a key first to list its models."
        ))
        return()
      }
      model_note(list(ok = FALSE, msg = "Fetching models..."))
      ids <- tryCatch(lister(src, key), error = function(e) NULL)
      if (is.null(ids) || length(ids) == 0) {
        model_note(list(
          ok = FALSE,
          msg = paste(
            "Couldn't fetch models for this key -- showing suggestions.",
            "Type an id if needed."
          )
        ))
        return()
      }
      updateSelectizeInput(
        session,
        "model",
        choices = ids,
        selected = isolate(trimws(input$model %||% "")),
        server = FALSE
      )
      model_note(list(
        ok = TRUE,
        msg = paste0(
          length(ids),
          " models loaded live from ",
          .tahoe_agent_provider_meta(src)$label,
          "."
        )
      ))
    })

    observeEvent(input$connect, {
      src <- input$source
      if (is.null(src) || identical(src, "shared")) {
        return()
      }
      key <- trimws(input$api_key %||% "")
      # Fall back to a key already in the environment so the user can connect
      # without pasting one.
      if (!nzchar(key)) {
        key <- .tahoe_agent_provider_env_key(src)
      }
      if (!nzchar(key)) {
        status(list(
          ok = FALSE,
          msg = "Paste an API key, or set the provider's environment variable."
        ))
        return()
      }
      model <- trimws(input$model %||% "")
      label <- .tahoe_agent_provider_meta(src)$label
      set_client(
        tahoe_agent_byok_credential(src, key, model),
        paste0("Connected: ", label, "."),
        secret = key
      )
      active_secret(key)
      # Clear the visible key field; the built client already holds what it needs.
      updateTextInput(session, "api_key", value = "")
    })

    observeEvent(input$forget, {
      client(NULL)
      active_secret("")
      updateTextInput(session, "api_key", value = "")
      status(list(ok = FALSE, msg = "Key forgotten. Enter a key and Connect."))
      do_clear()
    })

    observeEvent(input$chat_user_input, {
      msg <- input$chat_user_input
      if (is.null(msg) || !nzchar(trimws(msg))) {
        return()
      }
      cl <- client()
      if (is.null(cl)) {
        do_append("Select a model source and connect before chatting.")
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
      secret <- active_secret()

      # Guard BOTH failure paths so a provider error (bad key, quota, unknown
      # model, overload) surfaces as a chat message instead of an unhandled
      # observer error -- which would tear down the Shiny session:
      #   * stream_async() / chat_append() throwing synchronously -> tryCatch
      #   * the streaming promise rejecting mid-turn -> onRejected
      p <- tryCatch(
        do_append(cl$stream_async(msg)),
        error = function(e) {
          do_append(.tahoe_agent_friendly_error(conditionMessage(e), secret))
          NULL
        }
      )

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
            do_append(.tahoe_agent_friendly_error(
              conditionMessage(err),
              secret
            ))
          }
        )
      }
    })

    output$key_help <- renderUI({
      src <- input$source
      if (is.null(src) || identical(src, "shared")) {
        return(NULL)
      }
      meta <- .tahoe_agent_provider_meta(src)
      env_found <- nzchar(.tahoe_agent_provider_env_key(src))
      tagList(
        if (!is.null(meta$key_url)) {
          tags$p(
            class = "small mb-1",
            tags$a(
              href = meta$key_url,
              target = "_blank",
              rel = "noopener",
              "Get a key"
            ),
            " for ",
            meta$label,
            "."
          )
        },
        if (env_found) {
          tags$p(
            class = "small text-success mb-1",
            "✓ A key was found in your environment — leave the field",
            " blank and just pick a model and Connect."
          )
        }
      )
    })

    output$model_source <- renderUI({
      note <- model_note()
      if (is.null(note)) {
        return(NULL)
      }
      cls <- if (isTRUE(note$ok)) "text-success" else "text-muted"
      div(class = paste("small mb-2", cls), note$msg)
    })

    output$cred_status <- renderUI({
      st <- status()
      cls <- if (isTRUE(st$ok)) "text-success" else "text-muted"
      div(class = paste("small mb-2", cls), st$msg)
    })

    invisible(list(
      client = client,
      n_turns = n_turns,
      tools_used = tools_used,
      status = status,
      model_note = model_note
    ))
  })
}
