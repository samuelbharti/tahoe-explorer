# Cell-line explorer module.
#
# Filterable view of the cell-line metadata table: a sidebar of filters drives
# a reactable of the matching cell lines plus a few summary charts, and embeds
# the reusable export module so a subset can be pulled for downstream analysis.
# Reads only the small cell-line metadata table, so it is fast.

# DepMap portal base URL; cell IDs (ACH-XXXXXX) link to their portal page.
.cell_line_depmap_url <- "https://depmap.org/portal/cell_line/"

# Distinct, sorted, non-missing values of one column of `df` for filter choices.
.cell_line_choices <- function(df, column) {
  if (!column %in% names(df)) {
    return(character())
  }
  vals <- df[[column]]
  vals <- vals[!is.na(vals) & nzchar(as.character(vals))]
  sort(unique(as.character(vals)))
}

# Horizontal bar chart of the value counts of one column of `df`. Precomputes
# counts into a clean data frame before plotting (mirrors overview_mod.R).
.cell_line_bar <- function(df, column, fill, top_n = 15) {
  validate(need(column %in% names(df), "Column not available"))
  counts <- sort(table(df[[column]]), decreasing = TRUE)
  counts <- utils::head(counts, top_n)
  validate(need(length(counts) > 0, "No data to plot"))
  plot_df <- data.frame(
    label = factor(names(counts), levels = rev(names(counts))),
    n = as.integer(counts)
  )
  ggplot2::ggplot(plot_df, ggplot2::aes(x = label, y = n)) +
    ggplot2::geom_col(fill = fill) +
    ggplot2::geom_text(
      ggplot2::aes(label = n),
      hjust = -0.15,
      size = 3.2
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0, 0.12))) +
    ggplot2::labs(x = NULL, y = "Count") +
    ggplot2::theme_minimal(base_size = 13)
}

cell_line_explorer_ui <- function(id) {
  ns <- NS(id)
  df <- tahoe_cell_line()
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Filters",
      width = 300,
      selectizeInput(
        ns("organ"),
        "Organ",
        choices = .cell_line_choices(df, "Organ"),
        multiple = TRUE,
        options = list(placeholder = "All organs")
      ),
      selectizeInput(
        ns("gene"),
        "Driver gene",
        choices = .cell_line_choices(df, "Driver_Gene_Symbol"),
        multiple = TRUE,
        options = list(placeholder = "All genes")
      ),
      selectizeInput(
        ns("var_type"),
        "Variant type",
        choices = .cell_line_choices(df, "Driver_VarType"),
        multiple = TRUE,
        options = list(placeholder = "All variant types")
      ),
      textInput(
        ns("cell_name"),
        "Cell name contains",
        placeholder = "e.g. SYNTH"
      )
    ),
    bslib::card(
      bslib::card_header("Matching cell lines"),
      subset_export_ui(ns("export")),
      tags$hr(),
      reactable::reactableOutput(ns("table"))
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Cell lines by organ"),
        plotOutput(ns("organ_plot"), height = 320)
      ),
      bslib::card(
        bslib::card_header("Top driver genes"),
        plotOutput(ns("gene_plot"), height = 320)
      )
    ),
    bslib::card(
      bslib::card_header("Variant-type breakdown"),
      plotOutput(ns("var_type_plot"), height = 300)
    )
  )
}

cell_line_explorer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    all_cell_lines <- reactive(tahoe_cell_line())

    filtered <- reactive({
      df <- all_cell_lines()

      # Each filter is applied only when the user has supplied a value, so an
      # empty control never restricts the result set.
      if (length(input$organ) > 0 && "Organ" %in% names(df)) {
        df <- df[df$Organ %in% input$organ, , drop = FALSE]
      }
      if (length(input$gene) > 0 && "Driver_Gene_Symbol" %in% names(df)) {
        df <- df[df$Driver_Gene_Symbol %in% input$gene, , drop = FALSE]
      }
      if (length(input$var_type) > 0 && "Driver_VarType" %in% names(df)) {
        df <- df[df$Driver_VarType %in% input$var_type, , drop = FALSE]
      }
      term <- input$cell_name
      if (!is.null(term) && nzchar(term) && "cell_name" %in% names(df)) {
        keep <- stringr::str_detect(
          df$cell_name,
          stringr::fixed(term, ignore_case = TRUE)
        )
        df <- df[!is.na(keep) & keep, , drop = FALSE]
      }
      df
    })

    output$table <- reactable::renderReactable({
      df <- filtered()
      validate(need(nrow(df) > 0, "No cell lines match the current filters."))

      # Turn the DepMap ID into a clickable portal link when present, keeping
      # the original ID text as the link label.
      col_defs <- list()
      if ("Cell_ID_DepMap" %in% names(df)) {
        col_defs[["Cell_ID_DepMap"]] <- reactable::colDef(
          name = "DepMap ID",
          html = TRUE,
          cell = function(value) {
            if (is.na(value) || !nzchar(as.character(value))) {
              return("")
            }
            url <- paste0(.cell_line_depmap_url, value)
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
        sortable = TRUE,
        highlight = TRUE,
        compact = TRUE,
        defaultPageSize = 10,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 25, 50)
      )
    })

    output$organ_plot <- renderPlot({
      df <- filtered()
      validate(need(nrow(df) > 0, "No cell lines match the current filters."))
      .cell_line_bar(df, "Organ", "#41ab5d")
    })

    output$gene_plot <- renderPlot({
      df <- filtered()
      validate(need(nrow(df) > 0, "No cell lines match the current filters."))
      .cell_line_bar(df, "Driver_Gene_Symbol", "#2c7fb8")
    })

    output$var_type_plot <- renderPlot({
      df <- filtered()
      validate(need(nrow(df) > 0, "No cell lines match the current filters."))
      .cell_line_bar(df, "Driver_VarType", "#d95f0e")
    })

    subset_export_server(
      "export",
      data_reactive = filtered,
      file_stem = "cell_lines_subset"
    )

    # Expose the filtered reactive so callers (and tests) can consume it.
    filtered
  })
}
