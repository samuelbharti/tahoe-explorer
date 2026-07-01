# Drug & MOA explorer module.
#
# Browse the drug metadata table: filter by mechanism of action (broad/fine),
# approval status, clinical-trial status, target, and name; then view the
# filtered set as a table (with PubChem links) plus summary charts and export
# the current subset. Reads only the small drug metadata table, so it is fast.

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

# Horizontal bar chart of the value counts of `column` in `df`.
.drug_count_bar <- function(df, column, fill, top_n = 12) {
  validate(need(column %in% names(df), "Column not available"))
  validate(need(nrow(df) > 0, "No drugs match the current filters"))
  counts <- sort(table(df[[column]]), decreasing = TRUE)
  counts <- utils::head(counts, top_n)
  plot_df <- data.frame(
    label = factor(names(counts), levels = rev(names(counts))),
    n = as.integer(counts)
  )
  ggplot2::ggplot(plot_df, ggplot2::aes(x = label, y = n)) +
    ggplot2::geom_col(fill = fill) +
    ggplot2::geom_text(ggplot2::aes(label = n), hjust = -0.15, size = 3.2) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0, 0.12))) +
    ggplot2::labs(x = NULL, y = "Count") +
    ggplot2::theme_minimal(base_size = 13)
}

# Horizontal bar chart of the top-N most frequent targets in `df`.
.drug_target_bar <- function(df, fill, top_n = 12) {
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
    n = as.integer(counts)
  )
  ggplot2::ggplot(plot_df, ggplot2::aes(x = label, y = n)) +
    ggplot2::geom_col(fill = fill) +
    ggplot2::geom_text(ggplot2::aes(label = n), hjust = -0.15, size = 3.2) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0, 0.12))) +
    ggplot2::labs(x = NULL, y = "Drugs") +
    ggplot2::theme_minimal(base_size = 13)
}

drug_explorer_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Filters",
      width = 300,
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
      )
    ),
    bslib::layout_columns(
      col_widths = 12,
      bslib::card(
        bslib::card_header("Filtered drugs"),
        reactable::reactableOutput(ns("table"))
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Drugs by mechanism (MOA, broad)"),
        plotOutput(ns("moa_broad_plot"), height = 300)
      ),
      bslib::card(
        bslib::card_header("Approval status"),
        plotOutput(ns("approval_plot"), height = 300)
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Top targets"),
        plotOutput(ns("targets_plot"), height = 300)
      ),
      bslib::card(
        bslib::card_header("Export current subset"),
        subset_export_ui(ns("export"))
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

    output$table <- reactable::renderReactable({
      df <- filtered()
      validate(need(
        nrow(df) > 0,
        "No drugs match the current filters"
      ))
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
      reactable::reactable(
        df,
        columns = col_defs,
        searchable = TRUE,
        striped = TRUE,
        highlight = TRUE,
        compact = TRUE,
        defaultPageSize = 10,
        showPageSizeOptions = TRUE
      )
    })

    output$moa_broad_plot <- renderPlot({
      .drug_count_bar(filtered(), "moa-broad", "#2c7fb8")
    })

    output$approval_plot <- renderPlot({
      .drug_count_bar(filtered(), "human-approved", "#41ab5d")
    })

    output$targets_plot <- renderPlot({
      .drug_target_bar(filtered(), "#d95f0e")
    })

    subset_export_server(
      "export",
      data_reactive = filtered,
      file_stem = "drugs_subset"
    )

    # Return the filtered reactive so callers (and testServer) can read it.
    filtered
  })
}
