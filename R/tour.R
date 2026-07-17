# Guided product tours (cicerone).
#
# A click-through demo that walks a first-time user across a page's key
# controls. Tours are defined here and launched from the navbar "Demo" button
# (see server.R); each step targets a page module's namespaced element ids
# (the anchors are added in the module UI, e.g. drug_explorer_ui()).

#' The Drugs-page guided tour: filters -> picker -> table -> detail -> target
#' mutations -> charts -> export. Element ids are the drug module's namespaced
#' anchors (module id "drugs"). Returns a cicerone::Cicerone guide; call
#' `$init()` in the server and `$start()` to run it.
drug_tour <- function() {
  cicerone::Cicerone$new(id = "drug_tour")$step(
    el = "drugs-tour_filters",
    title = "Filter the catalog",
    description = paste(
      "Narrow the drug list by mechanism of action, approval status,",
      "clinical-trial phase, target gene, or name. Filters combine, and an",
      "empty filter means no restriction."
    ),
    position = "right"
  )$step(
    el = "drugs-tour_picker",
    title = "Pick a drug",
    description = paste(
      "Or jump straight to one drug here. This picker and the table's row",
      "selection always stay in sync."
    ),
    position = "right"
  )$step(
    el = "drugs-tour_table",
    title = "Browse the matches",
    description = paste(
      "Every filtered drug, with PubChem links. Click a row to select it and",
      "the whole right column updates to that drug. Use Columns to choose",
      "what to show."
    ),
    position = "top"
  )$step(
    el = "drugs-tour_detail",
    title = "Drug details",
    description = paste(
      "Targets, mechanism, approval, trials, and a PubChem link for the",
      "selected drug."
    ),
    position = "left"
  )$step(
    el = "drugs-tour_mut",
    title = "Target mutations",
    description = paste(
      "Which assayed cell lines carry a somatic variant in the drug's target",
      "genes — the lines where a mutant-vs-wild-type contrast could be",
      "designed."
    ),
    position = "left"
  )$step(
    el = "drugs-tour_charts",
    title = "Summary charts",
    description = paste(
      "Mechanism, approval, and top targets across the filtered set. The",
      "selected drug's categories are highlighted — and clicking any bar",
      "filters the table by that category."
    ),
    position = "left"
  )$step(
    el = "drugs-tour_export",
    title = "Export the subset",
    description = paste(
      "Download the current filtered drugs as CSV or Parquet for downstream",
      "analysis."
    ),
    position = "left"
  )
}
