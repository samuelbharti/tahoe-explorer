# The guided-demo tours are pure UI wiring. These tests verify that every page
# has a tour, that each tour is a cicerone guide, and that every step points at
# an anchor id that actually exists in that page's UI -- so a tour can never
# silently target a missing element.

# Page id -> a function returning that page's module UI (module id == page id).
.tour_page_ui <- list(
  overview = function() overview_ui("overview"),
  about = function() about_ui("about"),
  drugs = function() drug_explorer_ui("drugs"),
  cell_lines = function() cell_line_explorer_ui("cell_lines"),
  obs = function() obs_explorer_ui("obs"),
  coverage = function() coverage_ui("coverage"),
  qc = function() qc_ui("qc"),
  subset = function() subset_builder_ui("subset"),
  chat = function() chat_agent_ui("chat")
)

# The element ids ("#page-anchor") a guide's steps target.
.tour_step_els <- function(guide) {
  vapply(
    guide$.__enclos_env__$private$steps,
    function(s) s$element,
    character(1)
  )
}

test_that("every registered page has a tour", {
  expect_setequal(names(tahoe_tours()), names(app_pages()))
})

test_that("each tour is a cicerone guide with steps", {
  for (build in tahoe_tours()) {
    guide <- build()
    expect_s3_class(guide, "Cicerone")
    expect_gt(length(.tour_step_els(guide)), 0)
  }
})

test_that("every tour step targets an anchor present in its page UI", {
  # The Chat UI is conditional (assistant disabled in the hermetic test env
  # renders only the intro), so its enabled-only anchors can't be asserted from
  # the static UI; check those separately below.
  for (page in setdiff(names(tahoe_tours()), "chat")) {
    guide <- tahoe_tours()[[page]]()
    html <- as.character(.tour_page_ui[[page]]())
    for (el in .tour_step_els(guide)) {
      id <- sub("^#", "", el)
      expect_match(
        html,
        paste0('id="', id, '"'),
        fixed = TRUE,
        info = paste(page, "->", el)
      )
    }
  }
})

test_that("the chat tour intro anchor is always rendered", {
  html <- as.character(chat_agent_ui("chat"))
  expect_match(html, 'id="chat-tour_intro"', fixed = TRUE)
  # The chat tour still references the model picker and the chat window.
  expect_setequal(
    .tour_step_els(chat_tour()),
    c("#chat-tour_intro", "#chat-tour_source", "#chat-chat")
  )
})
