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
        plotly::plotlyOutput(ns("moa_plot"), height = 320)
      ),
      bslib::card(
        bslib::card_header("Cell lines by organ"),
        plotly::plotlyOutput(ns("organ_plot"), height = 320)
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
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0, 0.12))) +
    ggplot2::labs(x = NULL, y = "Count") +
    tahoe_theme()
}

overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    counts <- reactive(tahoe_summary_counts())

    output$summary_boxes <- renderUI({
      cc <- counts()
      fmt <- function(x) {
        if (is.null(x) || is.na(x)) "—" else format(x, big.mark = ",")
      }
      fmt_big <- function(x) {
        if (is.null(x) || is.na(x)) {
          return("—")
        }
        scales::label_number(
          accuracy = 0.1,
          scale_cut = scales::cut_short_scale()
        )(x)
      }
      # Cells and cell lines are computed from the obs data; note where from,
      # since the annotation tables cannot report them accurately.
      source_note <- switch(
        cc$obs_source,
        local = "Cells and cell lines from your local obs file.",
        remote = paste(
          "Cells and cell lines computed from the remote obs file",
          "(HuggingFace)."
        ),
        fixture = paste(
          "Showing synthetic demo fixtures — download the metadata for",
          "real numbers (see the README)."
        ),
        cc$obs_source
      )
      tagList(
        bslib::layout_columns(
          fill = FALSE,
          bslib::value_box("Cells", fmt_big(cc$cells), theme = "primary"),
          bslib::value_box("Cell lines", fmt(cc$cell_lines), theme = "primary"),
          bslib::value_box("Drugs", fmt(cc$drugs), theme = "primary"),
          bslib::value_box("Samples", fmt(cc$samples), theme = "secondary"),
          bslib::value_box("Plates", fmt(cc$plates), theme = "secondary"),
          bslib::value_box("Genes", fmt(cc$genes), theme = "secondary")
        ),
        p(class = "text-muted small mt-2", source_note)
      )
    })

    output$moa_plot <- plotly::renderPlotly({
      tahoe_plotly(.overview_bar(
        tahoe_drug(),
        "moa-broad",
        tahoe_colors$primary
      ))
    })

    output$organ_plot <- plotly::renderPlotly({
      # One row per cell line, so organs count distinct cell lines rather than
      # the driver-level rows of the source table.
      tahoe_plotly(
        .overview_bar(tahoe_cell_line_unique(), "Organ", tahoe_colors$green)
      )
    })
  })
}
