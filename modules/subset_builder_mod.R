# Subset builder module.
#
# Pick a slice across drug x cell-line x dose x plate, preview how many
# samples/cells it covers, then export the slice AND a copy-paste analysis
# recipe (R + Python) to reproduce the selection on the full Tahoe-100M dataset.
#
# The small `sample_metadata` table only carries drug / dose / plate, so those
# three dimensions narrow the matched-sample preview; cell lines exist only in
# the cell-level obs data, so a cell-line selection feeds the coverage estimate
# and the recipe predicate but does not restrict the sample preview.

subset_builder_ui <- function(id) {
  ns <- NS(id)

  samples <- tahoe_sample()
  drugs <- tryCatch(sort(unique(tahoe_drug()$drug)), error = function(e) NULL)
  cell_lines <- tryCatch(
    sort(unique(tahoe_cell_line()$cell_name)),
    error = function(e) NULL
  )
  doses <- tryCatch(
    {
      conc <- tahoe_parse_dose(samples$drugname_drugconc)$conc
      sort(unique(conc[!is.na(conc)]))
    },
    error = function(e) NULL
  )
  plates <- if ("plate" %in% names(samples)) {
    sort(unique(samples$plate))
  } else {
    NULL
  }

  remote <- identical(tahoe_obs_source()$type, "remote")

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Build a subset",
      width = 320,
      selectizeInput(
        ns("drugs"),
        "Drugs",
        choices = drugs,
        multiple = TRUE,
        options = list(placeholder = "All drugs")
      ),
      selectizeInput(
        ns("cell_lines"),
        "Cell lines",
        choices = cell_lines,
        multiple = TRUE,
        options = list(placeholder = "All cell lines")
      ),
      selectizeInput(
        ns("doses"),
        "Dose (concentration)",
        choices = doses,
        multiple = TRUE,
        options = list(placeholder = "All doses")
      ),
      selectizeInput(
        ns("plates"),
        "Plates",
        choices = plates,
        multiple = TRUE,
        options = list(placeholder = "All plates")
      ),
      tags$hr(),
      actionButton(
        ns("reset"),
        "Reset selection",
        class = "btn-sm btn-outline-secondary"
      ),
      if (remote) {
        tagList(
          tags$hr(),
          tags$p(
            class = "text-muted small",
            "Cell counts query the remote dataset on demand."
          ),
          actionButton(
            ns("estimate_cells"),
            "Estimate cell count",
            class = "btn-sm btn-primary"
          )
        )
      }
    ),
    uiOutput(ns("coverage_boxes")),
    bslib::card(
      bslib::card_header("Matched samples"),
      reactable::reactableOutput(ns("preview"))
    ),
    bslib::card(
      bslib::card_header("Export subset and analysis recipe"),
      subset_export_ui(ns("export"), show_recipe = TRUE)
    )
  )
}

# Format an integer for a value box, with an em dash for NA / unknown.
.subset_fmt <- function(x) {
  if (length(x) != 1 || is.na(x)) {
    "—"
  } else {
    format(x, big.mark = ",")
  }
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
  paste0("[", paste(sprintf('"%s"', x), collapse = ", "), "]")
}

# Render a numeric vector as a Python list literal, e.g. [0.05, 5].
.subset_py_num <- function(x) {
  paste0("[", paste(format(x, trim = TRUE), collapse = ", "), "]")
}

subset_builder_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    remote <- reactive(identical(tahoe_obs_source()$type, "remote"))

    sel_drugs <- reactive(input$drugs %||% character())
    sel_cell_lines <- reactive(input$cell_lines %||% character())
    sel_doses <- reactive({
      d <- suppressWarnings(as.numeric(input$doses))
      d[!is.na(d)]
    })
    sel_plates <- reactive(input$plates %||% character())

    observeEvent(input$reset, {
      updateSelectizeInput(session, "drugs", selected = character())
      updateSelectizeInput(session, "cell_lines", selected = character())
      updateSelectizeInput(session, "doses", selected = character())
      updateSelectizeInput(session, "plates", selected = character())
    })

    # Rows of the sample table matching the selected drug / dose / plate.
    # An empty selection means "no restriction" on that dimension. Cell lines
    # are absent from the sample table, so they do not narrow this set.
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

    # Cell-count estimate for the current selection. Sums the per-sample cell
    # counts from the cell-level obs data, filtered by the whitelisted drug /
    # plate dimensions. Returns NA on any error (e.g. remote unreachable).
    estimate_cells <- function() {
      res <- tahoe_obs_summary(
        "sample",
        filters = list(drug = sel_drugs(), plate = sel_plates()),
        metric = "n_cells",
        limit = NULL
      )
      if (!is.null(attr(res, "tahoe_error"))) {
        return(NA_integer_)
      }
      as.integer(sum(res$value, na.rm = TRUE))
    }

    # Local / fixture: compute reactively. Remote: gate behind the button so
    # nothing queries HuggingFace on load.
    cell_estimate <- reactive({
      if (remote()) {
        return(NA_integer_)
      }
      estimate_cells()
    })

    cell_estimate_remote <- eventReactive(input$estimate_cells, {
      estimate_cells()
    })

    n_cells <- reactive({
      if (remote()) {
        if (is.null(input$estimate_cells) || input$estimate_cells == 0) {
          NA_integer_
        } else {
          cell_estimate_remote()
        }
      } else {
        cell_estimate()
      }
    })

    output$coverage_boxes <- renderUI({
      cells_label <- if (remote() && is.na(n_cells())) {
        "Click to estimate"
      } else {
        .subset_fmt(n_cells())
      }
      bslib::layout_columns(
        fill = FALSE,
        bslib::value_box(
          "Drugs selected",
          .subset_fmt(length(sel_drugs())),
          theme = "primary"
        ),
        bslib::value_box(
          "Cell lines selected",
          .subset_fmt(length(sel_cell_lines())),
          theme = "primary"
        ),
        bslib::value_box(
          "Matched samples",
          .subset_fmt(nrow(matched_samples())),
          theme = "secondary"
        ),
        bslib::value_box(
          "Estimated cells",
          cells_label,
          theme = "info"
        )
      )
    })

    output$preview <- reactable::renderReactable({
      reactable::reactable(
        matched_samples(),
        searchable = TRUE,
        striped = TRUE,
        highlight = TRUE,
        compact = TRUE,
        defaultPageSize = 10,
        showPageSizeOptions = TRUE
      )
    })

    # A copy-paste analysis recipe reproducing the selection on the full
    # obs_metadata.parquet: an R (duckdb) snippet and a Python (pandas /
    # pyarrow) snippet, both embedding the concrete selected values.
    recipe <- reactive({
      drugs <- sel_drugs()
      cell_lines <- sel_cell_lines()
      doses <- sel_doses()
      plates <- sel_plates()

      if (
        length(drugs) == 0 &&
          length(cell_lines) == 0 &&
          length(doses) == 0 &&
          length(plates) == 0
      ) {
        return(paste(
          "No filters selected: this recipe would return the full dataset.",
          "Pick at least one drug, cell line, dose, or plate to build a",
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
      if (length(cell_lines) > 0) {
        r_where <- c(
          r_where,
          sprintf("cell_name IN %s", .subset_sql_vec(cell_lines))
        )
        py_where <- c(
          py_where,
          sprintf('df["cell_name"].isin(%s)', .subset_py_list(cell_lines))
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
        # Doses are embedded in drugname_drugconc; match on the parsed value.
        r_where <- c(
          r_where,
          sprintf(
            "TRY_CAST(regexp_extract(drugname_drugconc, ',\\s*([0-9.eE+-]+)\\s*,', 1) AS DOUBLE) IN %s",
            paste0("(", paste(format(doses, trim = TRUE), collapse = ", "), ")")
          )
        )
        py_where <- c(
          py_where,
          sprintf(
            'df["drugname_drugconc"].str.extract(r",\\s*([0-9.eE+-]+)\\s*,")[0].astype(float).isin(%s)',
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
        "# scanpy / AnnData: the same boolean mask filters an AnnData's .obs,",
        "# e.g.  adata = adata[subset.index]  (align on the obs index /",
        "# BARCODE), or read the .h5ad and subset adata[adata.obs eval(...)].",
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

    list(matched_samples = matched_samples, recipe = recipe)
  })
}
