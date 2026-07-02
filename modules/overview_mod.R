# Overview module.
#
# Landing tab: headline dataset counts as value boxes plus two quick summary
# charts. Reads only the small metadata tables, so it is fast.

overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("summary_boxes")),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Drugs by mechanism (MOA, broad)"),
        plotOutput(ns("moa_plot"), height = 320)
      ),
      bslib::card(
        bslib::card_header("Cell lines by organ"),
        plotOutput(ns("organ_plot"), height = 320)
      )
    )
  )
}

# Horizontal bar chart of the value counts of one column of `df`.
.overview_bar <- function(df, column, fill, top_n = 12) {
  validate(need(column %in% names(df), "Column not available"))
  counts <- sort(table(df[[column]]), decreasing = TRUE)
  counts <- utils::head(counts, top_n)
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

overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    counts <- reactive(tahoe_summary_counts())

    output$summary_boxes <- renderUI({
      cc <- counts()
      obs_label <- switch(
        cc$obs_source,
        local = "local file",
        remote = "remote (HuggingFace)",
        fixture = "demo fixture",
        cc$obs_source
      )
      data_label <- if (identical(cc$data_source, "real")) {
        "Downloaded metadata"
      } else {
        "Synthetic demo fixtures"
      }
      fmt <- function(x) {
        if (is.na(x)) "—" else format(x, big.mark = ",")
      }
      bslib::layout_columns(
        fill = FALSE,
        bslib::value_box("Drugs", fmt(cc$drugs), theme = "primary"),
        bslib::value_box("Cell lines", fmt(cc$cell_lines), theme = "primary"),
        bslib::value_box("Samples", fmt(cc$samples), theme = "secondary"),
        bslib::value_box("Plates", fmt(cc$plates), theme = "secondary"),
        bslib::value_box("Genes", fmt(cc$genes), theme = "secondary"),
        bslib::value_box(
          "Cell-level obs",
          obs_label,
          theme = "info",
          p(class = "small mb-0", data_label)
        )
      )
    })

    output$moa_plot <- renderPlot({
      .overview_bar(tahoe_drug(), "moa-broad", "#2c7fb8")
    })

    output$organ_plot <- renderPlot({
      # One row per cell line, so organs count distinct cell lines rather than
      # the driver-level rows of the source table.
      .overview_bar(tahoe_cell_line_unique(), "Organ", "#41ab5d")
    })
  })
}
