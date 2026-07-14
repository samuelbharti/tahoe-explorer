# Cross-module bridge: let the Chat assistant read and drive the Subset builder.
#
# The two features are separate self-registering pages (userInterface/*.R) that
# do not import each other. To connect them WITHOUT editing server.R / global.R,
# the Subset builder registers a tiny bridge -- get()/set() closures over ITS OWN
# module session -- into session$userData, which Shiny shares across module
# session proxies (the root session and every module see the same environment).
# The Chat server looks the bridge up LAZILY at tool-call time, so page mount
# order (subset is order 40, chat 45) does not matter.
#
# The bridge get()/set() run from the chat module's async tool-call continuation,
# NOT inside the subset module's reactive context, so the builder side isolates
# every reactive read and targets its own captured session for input writes (see
# modules/subset_builder_mod.R).

.tahoe_subset_bridge_key <- "tahoe_subset_bridge"

#' Register the Subset builder's live bridge for this Shiny session. `bridge` is a
#' list with two closures:
#'   * get() -> list(available, selection = <six-vector list>, estimated_cells,
#'       estimated_samples)
#'   * set(request) -> list(applied, selection, ignored, estimated_cells,
#'       estimated_samples)
#' No-ops when there is no session (a non-Shiny context), so callers need no
#' guard.
tahoe_register_subset_bridge <- function(session, bridge) {
  if (is.null(session) || is.null(session$userData)) {
    return(invisible(FALSE))
  }
  session$userData[[.tahoe_subset_bridge_key]] <- bridge
  invisible(TRUE)
}

#' The Subset builder bridge for this session, or NULL if the page is not mounted
#' (or the caller is outside a Shiny session).
tahoe_get_subset_bridge <- function(session) {
  if (is.null(session) || is.null(session$userData)) {
    return(NULL)
  }
  session$userData[[.tahoe_subset_bridge_key]]
}

#' Session-aware tools that let the assistant READ and DRIVE the interactive
#' Subset builder. Kept separate from tahoe_agent_tools() because they need the
#' Shiny `session` to find the bridge in userData; the chat server appends them to
#' the base suite. Each looks the bridge up at CALL time and degrades to a
#' friendly message (never an error) when the Subset builder is not present.
tahoe_subset_state_tools <- function(session) {
  str_arr <- function(desc) {
    list(type = "array", items = "string", desc = desc, required = FALSE)
  }
  list(
    list(
      name = "get_subset_selection",
      description = paste(
        "Read the user's CURRENT selection in the interactive Subset builder tab",
        "-- the six dimensions (organs, drivers, cell_lines, drugs, doses,",
        "plates) plus the estimated cells and samples it covers. Call this to see",
        "what the user has already picked before you advise on it or change it."
      ),
      arguments = list(),
      fun = function() {
        bridge <- tahoe_get_subset_bridge(session)
        if (is.null(bridge)) {
          return(list(
            available = FALSE,
            message = "The Subset builder is not loaded in this session."
          ))
        }
        tryCatch(
          bridge$get(),
          error = function(e) {
            list(
              available = FALSE,
              message = "Could not read the Subset builder state."
            )
          }
        )
      }
    ),
    list(
      name = "set_subset_selection",
      description = paste(
        "Drive the interactive Subset builder: set the user's selection across",
        "the six dimensions. Only the dimensions you pass are changed -- omit a",
        "dimension to leave it untouched, or pass an empty array to clear it.",
        "Values that don't exist in the dataset are ignored and reported back, so",
        "verify names with the list_* tools first. Prefer this over only printing",
        "a recipe whenever the user asks you to select, filter, or build a subset",
        "in the app; the Subset builder's preview and export then update live.",
        "Doses are in uM (0.05 / 0.5 / 5)."
      ),
      arguments = list(
        organs = str_arr("Tissues/organs to select."),
        drivers = str_arr("Driver genes to select."),
        cell_lines = str_arr("Exact cell-line names to select."),
        drugs = str_arr("Exact drug names to select."),
        doses = list(
          type = "array",
          items = "number",
          desc = "Doses in uM to select (0.05 / 0.5 / 5).",
          required = FALSE
        ),
        plates = str_arr("Plate ids to select.")
      ),
      fun = function(
        organs = NULL,
        drivers = NULL,
        cell_lines = NULL,
        drugs = NULL,
        doses = NULL,
        plates = NULL
      ) {
        bridge <- tahoe_get_subset_bridge(session)
        if (is.null(bridge)) {
          return(list(
            applied = FALSE,
            message = "The Subset builder is not loaded in this session."
          ))
        }
        request <- list(
          organs = organs,
          drivers = drivers,
          cell_lines = cell_lines,
          drugs = drugs,
          doses = doses,
          plates = plates
        )
        tryCatch(
          bridge$set(request),
          error = function(e) {
            list(
              applied = FALSE,
              message = "Could not update the Subset builder."
            )
          }
        )
      }
    )
  )
}
