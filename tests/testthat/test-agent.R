# Tests for the LLM assistant's hermetic surface: config gating, the system
# prompt assembly, the tool backing functions (against fixtures), and the chat
# module server driven with a stub client. No network, no Vertex credentials --
# setup.R sets TAHOE_AGENT_DISABLE=1 so the live path is never taken.

test_that("agent config accessors resolve env with sane defaults", {
  withr::local_envvar(
    TAHOE_VERTEX_MODEL = NA,
    GEMINI_MODEL = NA,
    TAHOE_AGENT_TEMPERATURE = NA,
    GEMINI_TEMPERATURE = NA,
    TAHOE_VERTEX_LOCATION = NA,
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

test_that("agent is unavailable without configuration and when disabled", {
  withr::local_envvar(
    TAHOE_AGENT_DISABLE = NA,
    TAHOE_VERTEX_PROJECT = NA,
    GOOGLE_CLOUD_PROJECT = NA,
    GCLOUD_PROJECT = NA
  )
  # No project id -> off, regardless of whether the packages are installed.
  expect_false(tahoe_agent_available())

  withr::local_envvar(TAHOE_VERTEX_PROJECT = "proj", TAHOE_AGENT_DISABLE = "1")
  # Kill switch wins even with a project configured.
  expect_false(tahoe_agent_available())
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

test_that("chat_agent_server streams a user turn to append with a stub client", {
  record <- new.env()
  record$sent <- character()
  record$appended <- list()
  stub_factory <- function() {
    list(stream_async = function(text) {
      record$sent <- c(record$sent, text)
      paste0("REPLY:", text)
    })
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
