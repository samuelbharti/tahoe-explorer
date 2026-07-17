# Drug & MOA explorer module.
#
# Browse the drug metadata table: filter by mechanism of action (broad/fine),
# approval status, clinical-trial status, target, and name; then view the
# filtered set as a table (with PubChem links) plus summary charts and export
# the current subset. Reads only the small drug metadata table, so it is fast.
#
# The page is cross-linked: selecting a drug row (or the sidebar picker -- the
# two stay in sync) drives a detail card and the target-mutation cross-reference,
# and highlights that drug's mechanism / approval / targets in the charts.
# Clicking a chart bar filters the table to that category (charts -> filters).

# Distinct, sorted, non-missing values of a column, or character(0) if absent.
.drug_choices <- function(df, column) {
  if (!column %in% names(df)) {
    return(character(0))
  }
  vals <- df[[column]]
  vals <- vals[!is.na(vals) & nzchar(as.character(vals))]
  sort(unique(as.character(vals)))
}

# Distinct targets, splitting the comma-separated `targets` column into atoms.
.drug_target_atoms <- function(df) {
  if (!"targets" %in% names(df)) {
    return(character(0))
  }
  atoms <- unlist(stringr::str_split(df[["targets"]], ","))
  atoms <- stringr::str_trim(atoms)
  atoms <- atoms[!is.na(atoms) & nzchar(atoms)]
  sort(unique(atoms))
}

# A single field value from a one-row drug data frame, or NA if the column is
# absent / empty. Used to build the detail card defensively.
.drug_field <- function(row, col) {
  if (is.null(row) || !col %in% names(row)) {
    return(NA_character_)
  }
  v <- row[[col]][[1]]
  if (is.null(v) || is.na(v) || !nzchar(as.character(v))) {
    return(NA_character_)
  }
  as.character(v)
}

# Shared horizontal-bar geom for the summary charts. `plot_df` carries `label`
# (a factor, reverse-ordered for coord_flip), `n`, and `hl` (logical: is this
# category part of the selected drug?). A `key` aesthetic makes each bar's
# category readable from a plotly click; highlighted bars get the accent color.
.drug_bar_geom <- function(plot_df, base_fill, y_lab) {
  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = label, y = n, key = label, fill = hl)
  ) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(
      values = c(`FALSE` = base_fill, `TRUE` = tahoe_colors$orange),
      guide = "none"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0, 0.12))) +
    ggplot2::labs(x = NULL, y = y_lab) +
    tahoe_theme()
}

# Horizontal bar chart of the value counts of `column` in `df`. `highlight` is a
# character vector of category values to emphasize (the selected drug's).
.drug_count_bar <- function(
  df,
  column,
  base_fill,
  highlight = character(0),
  top_n = 12
) {
  validate(need(column %in% names(df), "Column not available"))
  validate(need(nrow(df) > 0, "No drugs match the current filters"))
  counts <- sort(table(df[[column]]), decreasing = TRUE)
  counts <- utils::head(counts, top_n)
  plot_df <- data.frame(
    label = factor(names(counts), levels = rev(names(counts))),
    n = as.integer(counts),
    stringsAsFactors = FALSE
  )
  plot_df$hl <- as.character(plot_df$label) %in% highlight
  .drug_bar_geom(plot_df, base_fill, "Count")
}

# Horizontal bar chart of the top-N most frequent targets in `df`. `highlight`
# is a vector of target genes to emphasize (the selected drug's targets).
.drug_target_bar <- function(
  df,
  base_fill,
  highlight = character(0),
  top_n = 12
) {
  validate(need("targets" %in% names(df), "Targets not available"))
  validate(need(nrow(df) > 0, "No drugs match the current filters"))
  atoms <- unlist(stringr::str_split(df[["targets"]], ","))
  atoms <- stringr::str_trim(atoms)
  atoms <- atoms[!is.na(atoms) & nzchar(atoms)]
  validate(need(length(atoms) > 0, "No targets to summarize"))
  counts <- sort(table(atoms), decreasing = TRUE)
  counts <- utils::head(counts, top_n)
  plot_df <- data.frame(
    label = factor(names(counts), levels = rev(names(counts))),
    n = as.integer(counts),
    stringsAsFactors = FALSE
  )
  plot_df$hl <- as.character(plot_df$label) %in% highlight
  .drug_bar_geom(plot_df, base_fill, "Drugs")
}

# Build the selected-drug detail card body from a one-row drug data frame.
.drug_detail_ui <- function(row) {
  item <- function(label, value) {
    if (is.na(value)) {
      return(NULL)
    }
    div(
      class = "col-6 col-md-4 mb-2",
      div(
        class = "text-muted small text-uppercase",
        style = "letter-spacing:.03em;",
        label
      ),
      div(value)
    )
  }
  cid <- .drug_field(row, "pubchem_cid")
  pubchem <- if (!is.na(cid)) {
    div(
      class = "col-6 col-md-4 mb-2",
      div(
        class = "text-muted small text-uppercase",
        style = "letter-spacing:.03em;",
        "PubChem"
      ),
      tags$a(
        href = paste0("https://pubchem.ncbi.nlm.nih.gov/compound/", cid),
        target = "_blank",
        rel = "noopener",
        paste0("CID ", cid)
      )
    )
  } else {
    NULL
  }
  notes <- .drug_field(row, "gpt-notes-approval")
  tags$div(
    tags$h5(.drug_field(row, "drug"), class = "mb-2"),
    div(
      class = "row",
      item("Targets", .drug_field(row, "targets")),
      item("MOA (broad)", .drug_field(row, "moa-broad")),
      item("MOA (fine)", .drug_field(row, "moa-fine")),
      item("Human approved", .drug_field(row, "human-approved")),
      item("Clinical trials", .drug_field(row, "clinical-trials")),
      pubchem
    ),
    if (!is.na(notes)) div(class = "text-muted small mt-1", notes) else NULL
  )
}

drug_explorer_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Filters",
      width = 250,
      gap = "0.4rem",
      padding = "0.6rem",
      uiOutput(ns("filter_moa_broad")),
      uiOutput(ns("filter_moa_fine")),
      uiOutput(ns("filter_approval")),
      uiOutput(ns("filter_trials")),
      textInput(
        ns("target_search"),
        "Target contains",
        placeholder = "e.g. EGFR"
      ),
      textInput(
        ns("name_search"),
        "Drug name contains",
        placeholder = "e.g. Synthdrug"
      ),
      tags$hr(),
      selectizeInput(
        ns("focus_drug"),
        "Selected drug",
        choices = NULL,
        options = list(placeholder = "Pick a drug…")
      ),
      div(
        class = "text-muted small",
        "Click a table row or pick here — the two stay in sync."
      )
    ),
    # Two columns: the table (browse) on the left; the selected drug's detail,
    # target mutations, and summary charts + export stacked on the right, so a
    # row click updates that column top-to-bottom.
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Filtered drugs"),
          tahoe_table_columns_ui(ns("tbl"))
        ),
        div(
          class = "text-muted small px-1 pb-1",
          "Click a row to select a drug — its details, target mutations, and",
          " summary charts appear in the next column, with its mechanism /",
          " approval / targets highlighted. Click a chart bar to filter the table."
        ),
        tahoe_table_ui(ns("tbl"))
      ),
      div(
        bslib::card(
          bslib::card_header("Selected drug"),
          uiOutput(ns("drug_detail"))
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            class = "d-flex justify-content-between align-items-center",
            span("Target mutations in assayed cell lines"),
            div(
              class = "d-flex gap-2 align-items-center",
              tahoe_table_columns_ui(ns("muttbl")),
              .info_pop(
                paste(
                  "For the selected drug, the assayed cell lines that",
                  "carry a somatic variant in one of its target genes -- the",
                  "lines over which a target-mutant vs wild-type contrast could",
                  "be designed. Restricted to lines present in the obs grid;",
                  "variants from DepMap / Cellosaurus. Empty until variant data",
                  "is loaded."
                ),
                title = "Target mutations"
              )
            )
          ),
          uiOutput(ns("target_mut_caption")),
          tahoe_table_ui(ns("muttbl"))
        ),
        bslib::card(
          bslib::card_header("Drugs by mechanism (MOA, broad)"),
          plotly::plotlyOutput(ns("moa_broad_plot"), height = 260)
        ),
        bslib::card(
          bslib::card_header("Approval status"),
          plotly::plotlyOutput(ns("approval_plot"), height = 260)
        ),
        bslib::card(
          bslib::card_header("Top targets"),
          plotly::plotlyOutput(ns("targets_plot"), height = 260)
        ),
        bslib::card(
          bslib::card_header("Export current subset"),
          subset_export_ui(ns("export"))
        )
      )
    )
  )
}

drug_explorer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    drugs <- reactive(tahoe_drug())

    # Data-driven filter controls, built from the loaded table.
    output$filter_moa_broad <- renderUI({
      selectizeInput(
        ns("moa_broad"),
        "MOA (broad)",
        choices = .drug_choices(drugs(), "moa-broad"),
        multiple = TRUE,
        options = list(placeholder = "All mechanisms")
      )
    })
    output$filter_moa_fine <- renderUI({
      selectizeInput(
        ns("moa_fine"),
        "MOA (fine)",
        choices = .drug_choices(drugs(), "moa-fine"),
        multiple = TRUE,
        options = list(placeholder = "All fine mechanisms")
      )
    })
    output$filter_approval <- renderUI({
      selectizeInput(
        ns("approval"),
        "Human approved",
        choices = .drug_choices(drugs(), "human-approved"),
        multiple = TRUE,
        options = list(placeholder = "Any")
      )
    })
    output$filter_trials <- renderUI({
      selectizeInput(
        ns("trials"),
        "Clinical trials",
        choices = .drug_choices(drugs(), "clinical-trials"),
        multiple = TRUE,
        options = list(placeholder = "Any")
      )
    })

    # Apply every active filter defensively; an empty filter is no restriction.
    filtered <- reactive({
      df <- drugs()
      validate(need(nrow(df) > 0, "No drug metadata available"))

      keep_in <- function(df, column, selected) {
        if (is.null(selected) || length(selected) == 0) {
          return(df)
        }
        if (!column %in% names(df)) {
          return(df)
        }
        df[as.character(df[[column]]) %in% selected, , drop = FALSE]
      }

      df <- keep_in(df, "moa-broad", input$moa_broad)
      df <- keep_in(df, "moa-fine", input$moa_fine)
      df <- keep_in(df, "human-approved", input$approval)
      df <- keep_in(df, "clinical-trials", input$trials)

      target_search <- input$target_search
      if (
        !is.null(target_search) &&
          nzchar(target_search) &&
          "targets" %in% names(df)
      ) {
        hit <- stringr::str_detect(
          df[["targets"]],
          stringr::fixed(target_search, ignore_case = TRUE)
        )
        df <- df[!is.na(hit) & hit, , drop = FALSE]
      }

      name_search <- input$name_search
      if (
        !is.null(name_search) &&
          nzchar(name_search) &&
          "drug" %in% names(df)
      ) {
        hit <- stringr::str_detect(
          df[["drug"]],
          stringr::fixed(name_search, ignore_case = TRUE)
        )
        df <- df[!is.na(hit) & hit, , drop = FALSE]
      }

      df
    })

    # The selected drug's full record (from the whole table, so it survives a
    # filter that would hide the row), or NULL when nothing is selected.
    selected_row <- reactive({
      foc <- input$focus_drug
      if (is.null(foc) || !nzchar(foc)) {
        return(NULL)
      }
      d <- drugs()
      r <- d[as.character(d$drug) == foc, , drop = FALSE]
      if (nrow(r) == 0) NULL else r[1, , drop = FALSE]
    })

    # Per-column overrides for the drugs table: render pubchem_cid as a link.
    drug_table_cols <- function(df) {
      col_defs <- list()
      if ("pubchem_cid" %in% names(df)) {
        col_defs[["pubchem_cid"]] <- reactable::colDef(
          name = "PubChem",
          html = TRUE,
          cell = function(value) {
            if (is.na(value) || !nzchar(as.character(value))) {
              return("")
            }
            url <- paste0(
              "https://pubchem.ncbi.nlm.nih.gov/compound/",
              value
            )
            sprintf(
              '<a href="%s" target="_blank" rel="noopener">%s</a>',
              url,
              value
            )
          }
        )
      }
      col_defs
    }

    # The drugs table, with a column chooser and single-row selection. SMILES
    # and the free-text approval notes are hidden by default (both appear in the
    # detail card). `default_selected` re-highlights the focus drug's row after a
    # filter re-render.
    tbl <- tahoe_table_server(
      "tbl",
      data = filtered,
      columns = drug_table_cols,
      page_size = 25,
      hidden = c("canonical_smiles", "gpt-notes-approval"),
      selection = "single",
      on_click = "select",
      default_selected = function(df) {
        foc <- isolate(input$focus_drug)
        if (is.null(foc) || !nzchar(foc)) {
          return(NULL)
        }
        i <- which(as.character(df$drug) == foc)
        if (length(i) == 1) i else NULL
      },
      empty_message = "No drugs match the current filters"
    )

    # --- Table <-> picker two-way sync ---------------------------------------
    # A row click updates the picker (the canonical selection); guard against a
    # no-op update so the two observers can't ping-pong.
    observeEvent(tbl$selected(), {
      sel <- tbl$selected()
      if (length(sel) == 0) {
        return()
      }
      df <- filtered()
      if (sel < 1 || sel > nrow(df)) {
        return()
      }
      drug <- as.character(df$drug[[sel]])
      if (!identical(drug, input$focus_drug %||% "")) {
        updateSelectizeInput(session, "focus_drug", selected = drug)
      }
    })

    # A picker change highlights the matching table row (isolate the state read
    # so this observer fires only on the picker, not on selection changes).
    observeEvent(input$focus_drug, {
      df <- filtered()
      idx <- which(as.character(df$drug) == (input$focus_drug %||% ""))
      cur <- isolate(tbl$selected())
      if (length(idx) == 1) {
        if (!identical(as.integer(cur), as.integer(idx))) {
          tbl$set_selected(idx)
        }
      } else if (length(cur) > 0) {
        tbl$set_selected(NA)
      }
    })

    # --- Chart -> filter (click a bar to filter the table) -------------------
    observeEvent(plotly::event_data("plotly_click", source = "drug_moa"), {
      k <- plotly::event_data("plotly_click", source = "drug_moa")$key
      if (is.null(k) || !nzchar(as.character(k))) {
        return()
      }
      updateSelectizeInput(
        session,
        "moa_broad",
        selected = union(input$moa_broad, as.character(k))
      )
    })
    observeEvent(plotly::event_data("plotly_click", source = "drug_approval"), {
      k <- plotly::event_data("plotly_click", source = "drug_approval")$key
      if (is.null(k) || !nzchar(as.character(k))) {
        return()
      }
      updateSelectizeInput(
        session,
        "approval",
        selected = union(input$approval, as.character(k))
      )
    })
    observeEvent(plotly::event_data("plotly_click", source = "drug_targets"), {
      k <- plotly::event_data("plotly_click", source = "drug_targets")$key
      if (is.null(k) || !nzchar(as.character(k))) {
        return()
      }
      updateTextInput(session, "target_search", value = as.character(k))
    })

    # --- Charts (highlight the selected drug's categories) -------------------
    output$moa_broad_plot <- plotly::renderPlotly({
      hl <- .drug_field(selected_row(), "moa-broad")
      tahoe_plotly(
        .drug_count_bar(
          filtered(),
          "moa-broad",
          tahoe_colors$primary,
          highlight = if (is.na(hl)) character(0) else hl
        ),
        source = "drug_moa"
      )
    })

    output$approval_plot <- plotly::renderPlotly({
      hl <- .drug_field(selected_row(), "human-approved")
      tahoe_plotly(
        .drug_count_bar(
          filtered(),
          "human-approved",
          tahoe_colors$green,
          highlight = if (is.na(hl)) character(0) else hl
        ),
        source = "drug_approval"
      )
    })

    output$targets_plot <- plotly::renderPlotly({
      tahoe_plotly(
        .drug_target_bar(
          filtered(),
          tahoe_colors$sand,
          highlight = focus_targets()
        ),
        source = "drug_targets"
      )
    })

    # Populate the picker with drugs that declare a target.
    observeEvent(drugs(), once = TRUE, {
      d <- drugs()
      choices <- if ("targets" %in% names(d)) {
        sort(unique(d$drug[
          !is.na(d$targets) & nzchar(as.character(d$targets))
        ]))
      } else {
        character(0)
      }
      updateSelectizeInput(
        session,
        "focus_drug",
        choices = choices,
        selected = if (length(choices)) choices[[1]] else character(0),
        server = TRUE
      )
    })

    output$drug_detail <- renderUI({
      r <- selected_row()
      if (is.null(r)) {
        return(div(
          class = "text-muted small",
          "Select a drug row (or use the sidebar picker) to see its details."
        ))
      }
      .drug_detail_ui(r)
    })

    focus_targets <- reactive(tahoe_drug_targets(input$focus_drug))
    target_hits <- reactive(tahoe_target_mutations(focus_targets()))

    output$target_mut_caption <- renderUI({
      genes <- focus_targets()
      if (length(genes) == 0) {
        return(div(
          class = "text-muted small mb-2",
          "Pick a drug with known targets in the sidebar to see which assayed",
          "cell lines carry a mutation in them."
        ))
      }
      n_lines <- dplyr::n_distinct(target_hits()$cell_name)
      n_assayed <- dplyr::n_distinct(tahoe_cell_grid()$cell_name)
      div(
        class = "small mb-2",
        tags$b(input$focus_drug),
        " targets ",
        tags$b(paste(genes, collapse = ", ")),
        sprintf(
          " — %d of %d assayed cell lines carry a somatic variant in %s.",
          n_lines,
          n_assayed,
          if (length(genes) == 1) "it" else "one of them"
        )
      )
    })

    # Ordered, column-trimmed view of the target-mutation hits for display.
    mut_display <- reactive({
      hits <- target_hits()
      if (is.null(hits) || nrow(hits) == 0) {
        return(hits)
      }
      pref <- c(
        "cell_name",
        "gene",
        "protein_change",
        "variant_type",
        "consequence",
        "source"
      )
      df <- hits[, intersect(pref, names(hits)), drop = FALSE]
      df[order(df$gene, df$cell_name), , drop = FALSE]
    })

    tahoe_table_server(
      "muttbl",
      data = mut_display,
      empty_message = paste(
        "No assayed cell line carries a mutation in this drug's target(s)."
      )
    )

    subset_export_server(
      "export",
      data_reactive = filtered,
      file_stem = "drugs_subset"
    )

    # Return the filtered reactive so callers (and testServer) can read it.
    filtered
  })
}
