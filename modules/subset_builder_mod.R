# Subset builder module.
#
# Plan an analysis by slicing the dataset across tissue/organ, driver mutation,
# cell line, drug, dose, and plate. Coverage value boxes and a live plot update
# as the selection changes, then export the matched samples and a copy-paste
# recipe (R + Python) to reproduce the slice on the full Tahoe-100M dataset.
#
# Cell counts come from tahoe_cell_grid() (a small cached per drug x cell line x
# plate x dose aggregate of the obs data), so everything updates live and
# locally without re-querying 100M cells. All cell lines are pooled into every
# sample, so organ / driver / cell-line filters change the cell count, not the
# number of samples.

# Distinct, sorted, non-missing character values of a column.
.subset_choices <- function(df, column) {
  if (is.null(df) || !column %in% names(df)) {
    return(character())
  }
  vals <- as.character(df[[column]])
  vals <- vals[!is.na(vals) & nzchar(vals)]
  sort(unique(vals))
}

subset_builder_ui <- function(id) {
  ns <- NS(id)

  # In async mode the grid may be a slow remote build, so we do NOT touch it at
  # UI-construction time (that would block startup for every session); the server
  # populates the grid-derived choices once the background task resolves. In the
  # default synchronous mode this reads the fast local/fixture grid as before.
  grid <- if (.tahoe_async_enabled()) {
    NULL
  } else {
    tryCatch(tahoe_cell_grid(), error = function(e) NULL)
  }
  lines <- tryCatch(tahoe_cell_line(), error = function(e) NULL)
  samples <- tryCatch(tahoe_sample(), error = function(e) NULL)

  assayed <- if (!is.null(grid)) unique(grid$cell_name) else character()
  organ_choices <- .subset_choices(grid, "organ")
  driver_choices <- if (!is.null(lines)) {
    .subset_choices(
      lines[lines$cell_name %in% assayed, , drop = FALSE],
      "Driver_Gene_Symbol"
    )
  } else {
    character()
  }
  drug_choices <- .subset_choices(grid, "drug")
  dose_choices <- sort(unique(grid$conc[!is.na(grid$conc)]))

  # Plate labels carry the number of distinct drug treatments per plate.
  plate_choices <- NULL
  if (!is.null(samples) && all(c("plate", "drug") %in% names(samples))) {
    per_plate <- tapply(samples$drug, samples$plate, function(d) {
      length(unique(d))
    })
    ord <- order(as.integer(gsub("\\D", "", names(per_plate))))
    per_plate <- per_plate[ord]
    plate_choices <- stats::setNames(
      names(per_plate),
      sprintf("%s · %d drugs", names(per_plate), per_plate)
    )
  }

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Build a subset",
      width = 330,
      selectizeInput(
        ns("organs"),
        "Tissue / organ",
        choices = organ_choices,
        multiple = TRUE,
        options = list(placeholder = "All tissues")
      ),
      selectizeInput(
        ns("drivers"),
        "Driver mutation (gene)",
        choices = driver_choices,
        multiple = TRUE,
        options = list(placeholder = "Any driver gene")
      ),
      selectizeInput(
        ns("cell_lines"),
        "Cell lines",
        choices = assayed,
        multiple = TRUE,
        options = list(placeholder = "All (matching tissue/driver)")
      ),
      selectizeInput(
        ns("drugs"),
        "Drugs",
        choices = drug_choices,
        multiple = TRUE,
        options = list(placeholder = "All drugs")
      ),
      selectizeInput(
        ns("doses"),
        "Dose (µM)",
        choices = dose_choices,
        multiple = TRUE,
        options = list(placeholder = "All doses")
      ),
      selectizeInput(
        ns("plates"),
        "Plates",
        choices = plate_choices,
        multiple = TRUE,
        options = list(placeholder = "All plates")
      ),
      tags$hr(),
      actionButton(
        ns("reset"),
        "Reset selection",
        class = "btn-sm btn-outline-secondary"
      )
    ),
    uiOutput(ns("coverage_boxes")),
    div(
      class = "text-muted small mb-2",
      tags$strong("How the experiment is laid out: "),
      "14 plates (96-well), each a batch of ~93–95 drug treatments; ",
      tags$code("DMSO_TF"),
      " is the vehicle control on every plate. Doses are 0.05 / 0.5 / 5 µM. ",
      "All 50 cell lines are pooled into every sample, so tissue / driver / ",
      "cell-line filters change the ",
      tags$em("cell count"),
      ", not the number of samples."
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header("Cells in selection, by cell line"),
        plotly::plotlyOutput(ns("live_plot"), height = 360)
      ),
      bslib::card(
        bslib::card_header("Matched samples"),
        reactable::reactableOutput(ns("preview"))
      )
    ),
    bslib::card(
      bslib::card_header("Export subset and analysis recipe"),
      uiOutput(ns("estimate")),
      subset_export_ui(ns("export"), show_recipe = TRUE)
    )
  )
}

# The subset-recipe constants and helpers (.subset_obs_hf,
# .subset_obs_bytes_per_cell, .subset_fmt, .subset_sql_vec, .subset_py_list,
# .subset_py_num) and the recipe builder (tahoe_subset_recipe) now live in
# R/subset_recipe.R, shared with the Chat assistant's build_subset_recipe tool.

# Compact format for large counts (e.g. 28.5M) so value boxes don't wrap.
.subset_fmt_big <- function(x) {
  if (length(x) != 1 || is.na(x)) {
    return("—")
  }
  scales::label_number(accuracy = 0.1, scale_cut = scales::cut_short_scale())(x)
}

subset_builder_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    driver_lines <- reactive(tahoe_cell_line())

    if (.tahoe_async_enabled()) {
      # Build the grid in a background worker so a slow remote scan never blocks
      # the main process. Reading $result() suspends dependent reactives (like
      # req()) until the build finishes, then yields the same tibble the sync
      # path returns; a worker failure re-throws here and surfaces as an error.
      grid_task <- tahoe_make_grid_task()
      grid_task$invoke()
      grid <- reactive(grid_task$result())

      # The UI built the grid-derived selectize inputs empty (see the UI note);
      # populate them once the grid resolves. tahoe_cell_line() is a small local
      # table, so the driver choices stay synchronous. Plate choices come from
      # tahoe_sample() and are already populated by the UI in both modes.
      observe({
        g <- grid()
        assayed <- unique(g$cell_name)
        dl <- driver_lines()
        driver_choices <- if (
          all(c("cell_name", "Driver_Gene_Symbol") %in% names(dl))
        ) {
          .subset_choices(
            dl[dl$cell_name %in% assayed, , drop = FALSE],
            "Driver_Gene_Symbol"
          )
        } else {
          character()
        }
        updateSelectizeInput(
          session,
          "organs",
          choices = .subset_choices(
            g,
            "organ"
          )
        )
        updateSelectizeInput(session, "drivers", choices = driver_choices)
        updateSelectizeInput(session, "cell_lines", choices = sort(assayed))
        updateSelectizeInput(
          session,
          "drugs",
          choices = .subset_choices(
            g,
            "drug"
          )
        )
        updateSelectizeInput(
          session,
          "doses",
          choices = sort(unique(g$conc[!is.na(g$conc)]))
        )
      })
    } else {
      grid <- reactive(tahoe_cell_grid())
    }

    sel_organs <- reactive(input$organs %||% character())
    sel_drivers <- reactive(input$drivers %||% character())
    sel_cell_lines <- reactive(input$cell_lines %||% character())
    sel_drugs <- reactive(input$drugs %||% character())
    sel_doses <- reactive({
      d <- suppressWarnings(as.numeric(input$doses))
      d[!is.na(d)]
    })
    sel_plates <- reactive(input$plates %||% character())

    # The current selection across the six subset dimensions, in the plain-list
    # shape the shared recipe logic (R/subset_recipe.R) expects.
    selection <- reactive(
      list(
        organs = sel_organs(),
        drivers = sel_drivers(),
        cell_lines = sel_cell_lines(),
        drugs = sel_drugs(),
        doses = sel_doses(),
        plates = sel_plates()
      )
    )

    # Cell lines implied by the tissue + driver + explicit-cell-line filters,
    # restricted to the assayed lines that actually appear in the data.
    matched_cell_names <- reactive(
      tahoe_subset_matched_lines(selection(), grid(), driver_lines())
    )

    # Keep the cell-line picker in sync with the tissue / driver filters.
    observeEvent(
      list(sel_organs(), sel_drivers()),
      {
        g <- grid()
        choices <- sort(unique(g$cell_name))
        if (length(sel_organs()) > 0 && "organ" %in% names(g)) {
          choices <- sort(unique(g$cell_name[g$organ %in% sel_organs()]))
        }
        if (length(sel_drivers()) > 0) {
          dl <- driver_lines()
          hit <- unique(dl$cell_name[dl$Driver_Gene_Symbol %in% sel_drivers()])
          choices <- intersect(choices, hit)
        }
        updateSelectizeInput(
          session,
          "cell_lines",
          choices = choices,
          selected = intersect(sel_cell_lines(), choices)
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(input$reset, {
      for (ctrl in c(
        "organs",
        "drivers",
        "cell_lines",
        "drugs",
        "doses",
        "plates"
      )) {
        updateSelectizeInput(session, ctrl, selected = character())
      }
    })

    # The cell grid narrowed by every active filter. Empty on a dimension means
    # no restriction there.
    grid_filtered <- reactive({
      g <- grid()
      if (nrow(g) == 0) {
        return(g)
      }
      g <- g[g$cell_name %in% matched_cell_names(), , drop = FALSE]
      if (length(sel_drugs()) > 0) {
        g <- g[g$drug %in% sel_drugs(), , drop = FALSE]
      }
      if (length(sel_doses()) > 0) {
        g <- g[!is.na(g$conc) & g$conc %in% sel_doses(), , drop = FALSE]
      }
      if (length(sel_plates()) > 0) {
        g <- g[g$plate %in% sel_plates(), , drop = FALSE]
      }
      g
    })

    # Sample-level slice for the table and export: samples matching the drug /
    # dose / plate dimensions (cell lines are pooled across all samples).
    matched_samples <- reactive({
      df <- tahoe_sample()
      keep <- rep(TRUE, nrow(df))
      if (length(sel_drugs()) > 0 && "drug" %in% names(df)) {
        keep <- keep & df$drug %in% sel_drugs()
      }
      if (length(sel_plates()) > 0 && "plate" %in% names(df)) {
        keep <- keep & df$plate %in% sel_plates()
      }
      if (length(sel_doses()) > 0 && "drugname_drugconc" %in% names(df)) {
        conc <- tahoe_parse_dose(df$drugname_drugconc)$conc
        keep <- keep & !is.na(conc) & conc %in% sel_doses()
      }
      df[keep, , drop = FALSE]
    })

    output$coverage_boxes <- renderUI({
      g <- grid_filtered()
      n_cells <- as.integer(sum(g$n_cells, na.rm = TRUE))
      n_lines <- dplyr::n_distinct(g$cell_name)
      n_drugs <- dplyr::n_distinct(g$drug)
      n_samples <- nrow(matched_samples())
      bslib::layout_columns(
        fill = FALSE,
        bslib::value_box(
          "Cells",
          .subset_fmt_big(n_cells),
          theme = "primary"
        ),
        bslib::value_box(
          "Cell lines",
          .subset_fmt(n_lines),
          theme = "primary"
        ),
        bslib::value_box(
          "Drugs",
          .subset_fmt(n_drugs),
          theme = "secondary"
        ),
        bslib::value_box(
          "Samples",
          .subset_fmt(n_samples),
          theme = "secondary"
        )
      )
    })

    output$live_plot <- plotly::renderPlotly({
      g <- grid_filtered()
      validate(need(
        nrow(g) > 0 && sum(g$n_cells, na.rm = TRUE) > 0,
        "No cells match the current selection."
      ))
      by_line <- stats::aggregate(n_cells ~ cell_name, data = g, FUN = sum)
      by_line <- by_line[order(-by_line$n_cells), , drop = FALSE]
      by_line <- utils::head(by_line, 25)
      by_line$cell_name <- factor(
        by_line$cell_name,
        levels = rev(by_line$cell_name)
      )
      p <- ggplot2::ggplot(
        by_line,
        ggplot2::aes(x = .data$cell_name, y = .data$n_cells)
      ) +
        ggplot2::geom_col(fill = tahoe_colors$primary) +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(
          labels = scales::label_number(
            scale_cut = scales::cut_short_scale()
          )
        ) +
        ggplot2::labs(x = NULL, y = "Cells") +
        tahoe_theme()
      tahoe_plotly(p)
    })

    output$preview <- reactable::renderReactable({
      tahoe_reactable(matched_samples(), page_size = 8)
    })

    # Estimated size of the subset: cells (from the grid), matched samples, and
    # the approximate obs metadata to scan. Expression is separate and larger.
    estimate <- reactive({
      cells <- as.integer(sum(grid_filtered()$n_cells, na.rm = TRUE))
      list(
        cells = cells,
        samples = nrow(matched_samples()),
        obs_mb = round(cells * .subset_obs_bytes_per_cell / 1e6)
      )
    })

    output$estimate <- renderUI({
      e <- estimate()
      div(
        class = "text-muted small mb-2",
        tags$strong("Estimated pull: "),
        sprintf(
          paste(
            "~%s cells across %s samples · ~%s MB of obs metadata to scan.",
            "The expression matrix is downloaded separately and is much larger."
          ),
          .subset_fmt(e$cells),
          .subset_fmt(e$samples),
          .subset_fmt(e$obs_mb)
        )
      )
    })

    # Copy-paste recipe reproducing the selection straight from HuggingFace,
    # built by the shared recipe logic in R/subset_recipe.R (also used by the
    # Chat assistant's build_subset_recipe tool). The tissue / driver filters are
    # resolved to their concrete cell_name set inside tahoe_subset_recipe().
    recipe <- reactive(
      tahoe_subset_recipe(
        selection(),
        grid(),
        driver_lines(),
        tahoe_sample()
      )$recipe
    )

    subset_export_server(
      "export",
      data_reactive = matched_samples,
      file_stem = "tahoe_subset",
      recipe = recipe
    )

    list(matched_samples = matched_samples, grid_filtered = grid_filtered)
  })
}
