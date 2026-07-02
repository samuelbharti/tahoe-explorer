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

  grid <- tryCatch(tahoe_cell_grid(), error = function(e) NULL)
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
        plotOutput(ns("live_plot"), height = 360)
      ),
      bslib::card(
        bslib::card_header("Matched samples"),
        reactable::reactableOutput(ns("preview"))
      )
    ),
    bslib::card(
      bslib::card_header("Export subset and analysis recipe"),
      subset_export_ui(ns("export"), show_recipe = TRUE)
    )
  )
}

# Format an integer for a value box, with an em dash for NA / unknown.
.subset_fmt <- function(x) {
  if (length(x) != 1 || is.na(x)) "—" else format(x, big.mark = ",")
}

# Compact format for large counts (e.g. 28.5M) so value boxes don't wrap.
.subset_fmt_big <- function(x) {
  if (length(x) != 1 || is.na(x)) {
    return("—")
  }
  scales::label_number(accuracy = 0.1, scale_cut = scales::cut_short_scale())(x)
}

# Render a character vector as a single-quoted SQL IN list, e.g. ('a', 'b').
.subset_sql_vec <- function(x) {
  vals <- paste(
    vapply(
      x,
      function(v) paste0("'", gsub("'", "''", v, fixed = TRUE), "'"),
      character(1)
    ),
    collapse = ", "
  )
  paste0("(", vals, ")")
}

# Render a character vector as a Python list literal, e.g. ["a", "b"].
.subset_py_list <- function(x) {
  esc <- gsub('"', '\\\\"', x, perl = TRUE)
  paste0("[", paste(sprintf('"%s"', esc), collapse = ", "), "]")
}

# Render a numeric vector as a Python list literal, e.g. [0.05, 5].
.subset_py_num <- function(x) {
  paste0("[", paste(format(x, trim = TRUE), collapse = ", "), "]")
}

subset_builder_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    grid <- reactive(tahoe_cell_grid())
    driver_lines <- reactive(tahoe_cell_line())

    sel_organs <- reactive(input$organs %||% character())
    sel_drivers <- reactive(input$drivers %||% character())
    sel_cell_lines <- reactive(input$cell_lines %||% character())
    sel_drugs <- reactive(input$drugs %||% character())
    sel_doses <- reactive({
      d <- suppressWarnings(as.numeric(input$doses))
      d[!is.na(d)]
    })
    sel_plates <- reactive(input$plates %||% character())

    # Cell lines implied by the tissue + driver + explicit-cell-line filters,
    # restricted to the assayed lines that actually appear in the data.
    matched_cell_names <- reactive({
      g <- grid()
      names_out <- unique(g$cell_name)
      if (length(sel_organs()) > 0 && "organ" %in% names(g)) {
        names_out <- intersect(
          names_out,
          unique(g$cell_name[g$organ %in% sel_organs()])
        )
      }
      if (length(sel_drivers()) > 0) {
        dl <- driver_lines()
        if (all(c("cell_name", "Driver_Gene_Symbol") %in% names(dl))) {
          hit <- unique(dl$cell_name[dl$Driver_Gene_Symbol %in% sel_drivers()])
          names_out <- intersect(names_out, hit)
        }
      }
      if (length(sel_cell_lines()) > 0) {
        names_out <- intersect(names_out, sel_cell_lines())
      }
      names_out
    })

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

    output$live_plot <- renderPlot({
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
      ggplot2::ggplot(
        by_line,
        ggplot2::aes(x = .data$cell_name, y = .data$n_cells)
      ) +
        ggplot2::geom_col(fill = "#0b7285") +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(
          labels = scales::label_number(
            scale_cut = scales::cut_short_scale()
          )
        ) +
        ggplot2::labs(x = NULL, y = "Cells") +
        ggplot2::theme_minimal(base_size = 13)
    })

    output$preview <- reactable::renderReactable({
      reactable::reactable(
        matched_samples(),
        searchable = TRUE,
        striped = TRUE,
        highlight = TRUE,
        compact = TRUE,
        defaultPageSize = 8,
        showPageSizeOptions = TRUE
      )
    })

    # Copy-paste recipe reproducing the selection on obs_metadata.parquet. The
    # tissue / driver filters are resolved to their concrete cell_name set.
    recipe <- reactive({
      drugs <- sel_drugs()
      doses <- sel_doses()
      plates <- sel_plates()
      # Only constrain cell lines when the resolved set is a strict subset.
      all_lines <- unique(grid()$cell_name)
      lines <- matched_cell_names()
      constrain_lines <- length(lines) > 0 &&
        length(lines) < length(all_lines)

      if (
        length(drugs) == 0 &&
          length(doses) == 0 &&
          length(plates) == 0 &&
          !constrain_lines
      ) {
        return(paste(
          "No filters selected: this recipe would return the full dataset.",
          "Pick a tissue, driver, cell line, drug, dose, or plate to build a",
          "reproducible subset predicate."
        ))
      }

      r_where <- character()
      py_where <- character()
      if (length(drugs) > 0) {
        r_where <- c(r_where, sprintf("drug IN %s", .subset_sql_vec(drugs)))
        py_where <- c(
          py_where,
          sprintf('df["drug"].isin(%s)', .subset_py_list(drugs))
        )
      }
      if (constrain_lines) {
        r_where <- c(
          r_where,
          sprintf("cell_name IN %s", .subset_sql_vec(lines))
        )
        py_where <- c(
          py_where,
          sprintf('df["cell_name"].isin(%s)', .subset_py_list(lines))
        )
      }
      if (length(plates) > 0) {
        r_where <- c(r_where, sprintf("plate IN %s", .subset_sql_vec(plates)))
        py_where <- c(
          py_where,
          sprintf('df["plate"].isin(%s)', .subset_py_list(plates))
        )
      }
      if (length(doses) > 0) {
        r_where <- c(
          r_where,
          sprintf(
            paste0(
              "TRY_CAST(regexp_extract(drugname_drugconc, ",
              "',\\s*([0-9.eE+-]+)\\s*,', 1) AS DOUBLE) IN (%s)"
            ),
            paste(format(doses, trim = TRUE), collapse = ", ")
          )
        )
        py_where <- c(
          py_where,
          sprintf(
            paste0(
              'df["drugname_drugconc"].str.extract(',
              'r",\\s*([0-9.eE+-]+)\\s*,")[0].astype(float).isin(%s)'
            ),
            .subset_py_num(doses)
          )
        )
      }

      r_predicate <- paste(r_where, collapse = "\n    AND ")
      py_predicate <- paste(py_where, collapse = "\n    & ")

      r_snippet <- paste(
        "## R (duckdb) --------------------------------------------------",
        "library(duckdb); library(DBI)",
        "con <- dbConnect(duckdb())",
        "subset <- dbGetQuery(con, \"",
        "  SELECT *",
        "  FROM read_parquet('obs_metadata.parquet')",
        sprintf("  WHERE %s", r_predicate),
        "\")",
        "dbDisconnect(con, shutdown = TRUE)",
        sep = "\n"
      )
      py_snippet <- paste(
        "## Python (pandas / pyarrow) -----------------------------------",
        "import pandas as pd",
        "df = pd.read_parquet('obs_metadata.parquet')",
        sprintf("subset = df[(\n    %s\n)]", py_predicate),
        "",
        "# scanpy / AnnData: use the same mask on adata.obs, e.g.",
        "# adata = adata[subset.index]  (align on the obs index / BARCODE).",
        sep = "\n"
      )
      paste(r_snippet, "", py_snippet, sep = "\n")
    })

    subset_export_server(
      "export",
      data_reactive = matched_samples,
      file_stem = "tahoe_subset",
      recipe = recipe
    )

    list(matched_samples = matched_samples, grid_filtered = grid_filtered)
  })
}
