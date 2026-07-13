# Tests for the LLM assistant's hermetic surface: config gating, the system
# prompt assembly, provider/backend selection, the tool backing functions
# (against fixtures), and the chat module server driven with a stub client. No
# network, no credentials -- setup.R sets TAHOE_AGENT_DISABLE=1 so the live path
# is never taken; individual tests toggle env vars via withr::local_envvar.

test_that("agent config accessors resolve env with sane defaults", {
  withr::local_envvar(
    TAHOE_VERTEX_MODEL = NA,
    GEMINI_MODEL = NA,
    TAHOE_AGENT_TEMPERATURE = NA,
    GEMINI_TEMPERATURE = NA,
    TAHOE_VERTEX_LOCATION = NA,
    VERTEX_LOCATION = NA,
    GOOGLE_CLOUD_LOCATION = NA,
    GOOGLE_CLOUD_REGION = NA
  )
  expect_equal(tahoe_agent_model(), "gemini-2.5-flash")
  expect_equal(tahoe_agent_temperature(), 0.2)
  expect_equal(.tahoe_vertex_location(), "us-central1")

  withr::local_envvar(
    TAHOE_VERTEX_MODEL = "gemini-2.5-pro",
    TAHOE_AGENT_TEMPERATURE = "0"
  )
  expect_equal(tahoe_agent_model(), "gemini-2.5-pro")
  expect_equal(tahoe_agent_temperature(), 0)
})

test_that("gating splits the shared default from BYOK and honours the kill switch", {
  withr::local_envvar(
    TAHOE_AGENT_DISABLE = NA,
    TAHOE_VERTEX_PROJECT = NA,
    VERTEX_PROJECT_ID = NA,
    GOOGLE_CLOUD_PROJECT = NA,
    GCLOUD_PROJECT = NA,
    TAHOE_AGENT_BYOK = NA,
    TAHOE_AGENT_BYOK_PROVIDERS = NA
  )
  if (tahoe_agent_packages_ok()) {
    # No project id -> shared default off, but BYOK is on by default, so the
    # page is still enabled.
    expect_false(tahoe_agent_default_available())
    expect_false(tahoe_agent_available()) # back-compat alias == default path
    expect_true(tahoe_agent_byok_enabled())
    expect_true(tahoe_agent_enabled())
  } else {
    # Packages absent (e.g. CI): nothing is enabled.
    expect_false(tahoe_agent_enabled())
  }

  # The kill switch wins over everything.
  withr::local_envvar(TAHOE_AGENT_DISABLE = "1", TAHOE_VERTEX_PROJECT = "proj")
  expect_false(tahoe_agent_default_available())
  expect_false(tahoe_agent_byok_enabled())
  expect_false(tahoe_agent_enabled())
})

test_that("BYOK can be turned off and its providers are whitelisted", {
  skip_if_not(tahoe_agent_packages_ok())
  withr::local_envvar(TAHOE_AGENT_DISABLE = NA, TAHOE_AGENT_BYOK = "0")
  expect_false(tahoe_agent_byok_enabled())

  withr::local_envvar(
    TAHOE_AGENT_BYOK = "1",
    TAHOE_AGENT_BYOK_PROVIDERS = "gemini, bogus, anthropic"
  )
  expect_true(tahoe_agent_byok_enabled())
  # Unknown names are dropped; order follows the request.
  expect_equal(tahoe_agent_byok_providers(), c("gemini", "anthropic"))
})

test_that("system prompt assembles the context files and key facts", {
  sp <- tahoe_agent_system_prompt()
  expect_type(sp, "character")
  expect_gt(nchar(sp), 500)
  expect_true(grepl("Tahoe-100M", sp, fixed = TRUE))
  expect_true(grepl("subset", sp, ignore.case = TRUE))
  expect_true(grepl("decline", sp, ignore.case = TRUE))
})

test_that("tool suite is complete and every spec converts to an ellmer tool", {
  tools <- tahoe_agent_tools()
  nm <- vapply(tools, `[[`, "", "name")
  expect_true(all(
    c(
      "dataset_overview",
      "build_subset_recipe",
      "drug_target_mutants",
      "obs_summary"
    ) %in%
      nm
  ))
  # The conversion needs ellmer; skip where it is not installed (e.g. CI).
  skip_if_not_installed("ellmer")
  for (spec in tools) {
    expect_no_error(.tahoe_agent_ellmer_tool(spec))
  }
})

test_that("tahoe_agent_client selects the backend and passes key + model", {
  skip_if_not_installed("ellmer") # tahoe_agent_client uses ellmer::params()
  seen <- new.env()
  fake_backends <- list(
    gemini = function(system_prompt, params, echo, model = "", api_key = "") {
      seen$provider <- "gemini"
      seen$model <- model
      seen$api_key <- api_key
      seen$system_prompt <- system_prompt
      list(register_tool = function(...) invisible(NULL))
    }
  )
  cred <- tahoe_agent_byok_credential(
    "gemini",
    "sk-test-123",
    model = "gemini-x"
  )
  cl <- tahoe_agent_client(
    credential = cred,
    system_prompt = "SP",
    tools = list(),
    backends = fake_backends
  )
  expect_false(is.null(cl))
  expect_equal(seen$provider, "gemini")
  expect_equal(seen$api_key, "sk-test-123")
  expect_equal(seen$model, "gemini-x")
  expect_equal(seen$system_prompt, "SP")
})

test_that("tahoe_agent_client rejects a missing key and an unknown provider", {
  fake_backends <- list(
    gemini = function(...) list(register_tool = function(...) invisible(NULL))
  )
  # Empty key errors before any provider call (no ellmer needed).
  expect_error(
    tahoe_agent_client(
      credential = tahoe_agent_byok_credential("gemini", ""),
      tools = list(),
      backends = fake_backends
    ),
    "API key"
  )
  # Unknown provider is rejected -- a hostile source value can never reach a
  # backend that is not in the registry.
  expect_error(
    tahoe_agent_client(
      credential = list(kind = "byok", provider = "evil", api_key = "x"),
      tools = list(),
      backends = fake_backends
    ),
    "unknown chat provider"
  )
})

test_that("tool backing functions return capped, well-shaped results", {
  tools <- tahoe_agent_tools()
  by <- stats::setNames(tools, vapply(tools, `[[`, "", "name"))

  ov <- by[["dataset_overview"]]$fun()
  expect_true(all(c("drugs", "assayed_cell_lines", "cells") %in% names(ov)))
  expect_gt(ov$drugs, 0)

  ld <- by[["list_drugs"]]$fun(limit = 5)
  expect_true(all(c("total_matches", "returned", "rows") %in% names(ld)))
  expect_lte(ld$returned, 5)

  # obs_summary enforces the whitelist: a hallucinated column returns a message,
  # NOT an error that would abort the LLM turn.
  bad <- by[["obs_summary"]]$fun(group_by = "x'; DROP TABLE obs;--")
  expect_false(is.null(bad$error))
  ok <- by[["obs_summary"]]$fun(group_by = "drug", metric = "n_cells")
  expect_lte(ok$returned, 25L)

  # build_subset_recipe returns a recipe + estimate for a real drug.
  drug <- tahoe_drug()$drug[[1]]
  r <- by[["build_subset_recipe"]]$fun(drugs = drug)
  expect_true(grepl("Estimated subset", r$recipe))
  expect_type(r$estimated_cells, "integer")
})

test_that("chat_agent_server streams a shared-source turn to append", {
  record <- new.env()
  record$creds <- list()
  record$sent <- character()
  record$appended <- list()
  stub_factory <- function(credential) {
    record$creds[[length(record$creds) + 1L]] <- credential
    list(
      on_tool_request = function(cb) invisible(NULL),
      stream_async = function(text) {
        record$sent <- c(record$sent, text)
        paste0("REPLY:", text)
      }
    )
  }
  recorder <- function(response) {
    record$appended[[length(record$appended) + 1L]] <- response
    NULL
  }

  testServer(
    function(id) {
      chat_agent_server(id, client_factory = stub_factory, append = recorder)
    },
    {
      session$setInputs(source = "shared") # builds the client via factory()
      expect_equal(record$creds[[1]]$kind, "vertex")

      session$setInputs(chat_user_input = "How many drugs are there?")
      expect_true("How many drugs are there?" %in% record$sent)
      expect_gt(length(record$appended), 0)
      expect_equal(n_turns(), 1L)

      # Blank input is ignored (no extra turn).
      session$setInputs(chat_user_input = "   ")
      expect_equal(n_turns(), 1L)
    }
  )
})

test_that("chat_agent_server BYOK: connect carries the key, forget clears it", {
  record <- new.env()
  record$creds <- list()
  record$sent <- character()
  record$appended <- list()
  stub_factory <- function(credential) {
    record$creds[[length(record$creds) + 1L]] <- credential
    list(
      on_tool_request = function(cb) invisible(NULL),
      stream_async = function(text) {
        record$sent <- c(record$sent, text)
        paste0("REPLY:", text)
      }
    )
  }
  recorder <- function(response) {
    record$appended[[length(record$appended) + 1L]] <- response
    NULL
  }

  testServer(
    function(id) {
      chat_agent_server(id, client_factory = stub_factory, append = recorder)
    },
    {
      # Selecting a BYOK provider does NOT build a client yet.
      session$setInputs(source = "gemini")
      session$setInputs(chat_user_input = "hi")
      expect_equal(n_turns(), 0L)
      last <- record$appended[[length(record$appended)]]
      expect_true(grepl("connect", last, ignore.case = TRUE))

      # Connect a key -> client is built and the credential carries the key.
      session$setInputs(api_key = "sk-secret-XYZ", model = "")
      session$setInputs(connect = 1)
      expect_false(is.null(client()))
      cred <- record$creds[[length(record$creds)]]
      expect_equal(cred$kind, "byok")
      expect_equal(cred$provider, "gemini")
      expect_equal(cred$api_key, "sk-secret-XYZ")

      # The key must never surface in the status shown to the user.
      st <- status()
      expect_true(isTRUE(st$ok))
      expect_false(grepl("sk-secret-XYZ", st$msg, fixed = TRUE))

      # Now a turn streams through the connected client.
      session$setInputs(chat_user_input = "hello")
      expect_true("hello" %in% record$sent)
      expect_equal(n_turns(), 1L)

      # Forget drops the client (no more chatting until reconnect).
      session$setInputs(forget = 1)
      expect_true(is.null(client()))
    }
  )
})

test_that("friendly error mapper classifies failures and redacts the key", {
  f <- .tahoe_agent_friendly_error
  expect_match(
    f("API key not valid. Please pass a valid API key."),
    "key was rejected",
    ignore.case = TRUE
  )
  expect_match(
    f("429 RESOURCE_EXHAUSTED: quota exceeded"),
    "Rate limit or quota",
    ignore.case = TRUE
  )
  expect_match(f("503 model is overloaded"), "busy", ignore.case = TRUE)
  expect_match(
    f("404 model not found: bad"),
    "isn't available",
    ignore.case = TRUE
  )
  expect_match(
    f("400 invalid_argument"),
    "request was rejected",
    ignore.case = TRUE
  )
  # The key is redacted out of whatever text is surfaced.
  msg <- f("401 unauthorized for key sk-secret-123", secret = "sk-secret-123")
  expect_false(grepl("sk-secret-123", msg, fixed = TRUE))
})

test_that("provider model suggestions default sanely and honour env override", {
  withr::local_envvar(
    TAHOE_AGENT_MODELS_GEMINI = NA,
    TAHOE_AGENT_MODELS_ANTHROPIC = NA
  )
  expect_true("gemini-2.5-flash" %in% .tahoe_agent_provider_models("gemini"))
  expect_true("claude-sonnet-5" %in% .tahoe_agent_provider_models("anthropic"))
  expect_equal(.tahoe_agent_provider_models("nope"), character(0))

  withr::local_envvar(TAHOE_AGENT_MODELS_GEMINI = "m1, m2")
  expect_equal(.tahoe_agent_provider_models("gemini"), c("m1", "m2"))
})

test_that("chat_agent_server surfaces a provider error instead of crashing", {
  record <- new.env()
  record$appended <- list()
  # A client whose stream_async throws synchronously (e.g. an immediate 404).
  boom_factory <- function(credential) {
    list(
      on_tool_request = function(cb) invisible(NULL),
      stream_async = function(text) stop("404 model not found: nope")
    )
  }
  recorder <- function(response) {
    record$appended[[length(record$appended) + 1L]] <- response
    NULL
  }

  testServer(
    function(id) {
      chat_agent_server(id, client_factory = boom_factory, append = recorder)
    },
    {
      session$setInputs(source = "shared")
      # The turn must NOT error out of the observer (which would kill the
      # session) -- it appends a friendly, mapped message instead.
      expect_no_error(session$setInputs(chat_user_input = "hi"))
      last <- record$appended[[length(record$appended)]]
      expect_match(last, "isn't available", ignore.case = TRUE)
      expect_equal(n_turns(), 1L)
    }
  )
})
