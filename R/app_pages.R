# Page registry.
#
# Feature tabs self-register here instead of editing ui.R / server.R, so new
# explorer modules can be added in isolation (one userInterface/ file each) with
# no merge conflicts in the app entrypoints.
#
# The registry is kept in a process-global option (not an environment) so it is
# robust to this file being sourced several times during Shiny startup (Shiny
# autoloads R/ in addition to global.R sourcing it via load_components, and the
# sourcings can land in different environments).
#
# In a userInterface/<feature>_page.R file:
#
#   register_page(
#     id = "drugs",
#     title = "Drugs",
#     ui = drug_explorer_ui("drugs"),
#     server = function() drug_explorer_server("drugs"),
#     order = 10
#   )

#' Register a navigation page. `ui` is the page's UI (a tag/tagList); `server`
#' is a zero-argument function that mounts the page's module server(s); lower
#' `order` sorts earlier in the navbar. Re-registering the same `id` overwrites.
register_page <- function(id, title, ui, server = NULL, order = 100) {
  pages <- getOption("tahoe.pages", default = list())
  pages[[id]] <- list(
    id = id,
    title = title,
    ui = ui,
    server = server,
    order = order
  )
  options(tahoe.pages = pages)
  invisible()
}

#' Registered pages, sorted by `order`.
app_pages <- function() {
  pages <- getOption("tahoe.pages", default = list())
  if (length(pages) == 0) {
    return(pages)
  }
  ord <- vapply(pages, function(p) p$order, numeric(1))
  pages[order(ord)]
}

#' Build the list of bslib nav panels for the navbar. Names are stripped so the
#' panels splice into page_navbar() as unnamed (positional) arguments.
app_nav_panels <- function() {
  unname(lapply(app_pages(), function(p) {
    bslib::nav_panel(title = p$title, value = p$id, p$ui)
  }))
}

#' Mount every registered page's server (call inside the app server function).
mount_page_servers <- function() {
  for (p in app_pages()) {
    if (!is.null(p$server)) {
      p$server()
    }
  }
}
