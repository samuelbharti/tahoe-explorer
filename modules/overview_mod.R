# Overview module.
#
# Landing tab: headline dataset counts as value boxes, plus an interactive
# "cell lines by organ" chart linked to a cell-line metadata table — click an
# organ bar to drill the table down to that organ. Reads only the small
# metadata tables, so it is fast.

overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("summary_boxes")),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        bslib::card_header("Cell lines by organ"),
        plotly::plotlyOutput(ns("organ_plot"), height = 380),
        bslib::card_footer(
          class = "text-muted small",
          "Click an organ bar to filter the table."
        )
      ),
      bslib::card(
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          uiOutput(ns("table_title"), inline = TRUE),
          uiOutput(ns("clear_filter"), inline = TRUE)
        ),
        reactable::reactableOutput(ns("cell_line_table"))
      )
    )
  )
}

# Horizontal bar chart of cell-line counts per organ. `key = label` makes each
# bar carry its organ name so plotly click events can report which was clicked.
.overview_organ_bar <- function(df, top_n = 15) {
  validate(need("Organ" %in% names(df), "Organ column not available"))
  counts <- sort(table(df[["Organ"]]), decreasing = TRUE)
  counts <- utils::head(counts, top_n)
  plot_df <- data.frame(
    label = factor(names(counts), levels = rev(names(counts))),
    n = as.integer(counts)
  )
  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = label, y = n, key = label)
  ) +
    ggplot2::geom_col(fill = tahoe_colors$primary, width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0, 0.12))) +
    ggplot2::labs(x = NULL, y = "Cell lines") +
    tahoe_theme()
}

overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    organ_src <- session$ns("organ")
    # The annotation table is driver-level (~102 distinct cell lines), but only
    # the ~50 in the obs data were actually assayed. Restrict to those so the
    # chart and table reconcile with the "Cell lines" headline count. Falls
    # back to the full annotation set when no grid is available (e.g. fixtures).
    lines <- reactive({
      cl <- tahoe_cell_line_unique()
      assayed <- tryCatch(
        unique(tahoe_cell_grid()$cell_name),
        error = function(e) NULL
      )
      if (!is.null(assayed) && length(assayed) > 0) {
        keep <- cl$cell_name %in% assayed
        if (any(keep)) {
          cl <- cl[keep, , drop = FALSE]
        }
      }
      cl
    })
    selected_organ <- reactiveVal(NULL)

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
        grid = "Cells and cell lines from your local cell-count grid.",
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

    output$organ_plot <- plotly::renderPlotly({
      tahoe_plotly(
        .overview_organ_bar(lines()),
        tooltip = c("y"),
        source = organ_src
      )
    })

    # Clicking an organ bar filters the table; clicking the same one clears it.
    observeEvent(plotly::event_data("plotly_click", source = organ_src), {
      click <- plotly::event_data("plotly_click", source = organ_src)
      organ <- click$key
      if (is.null(organ) || !nzchar(organ)) {
        return()
      }
      if (identical(organ, selected_organ())) {
        selected_organ(NULL)
      } else {
        selected_organ(organ)
      }
    })

    output$table_title <- renderUI({
      organ <- selected_organ()
      if (is.null(organ)) {
        span("Cell lines — all organs")
      } else {
        tagList(
          "Cell lines — ",
          span(class = "badge text-bg-primary", organ)
        )
      }
    })

    output$clear_filter <- renderUI({
      if (is.null(selected_organ())) {
        return(NULL)
      }
      actionLink(session$ns("clear"), "Show all", class = "small")
    })

    observeEvent(input$clear, selected_organ(NULL))

    filtered_lines <- reactive({
      df <- lines()
      organ <- selected_organ()
      if (!is.null(organ)) {
        df <- df[!is.na(df$Organ) & df$Organ == organ, , drop = FALSE]
      }
      cols <- c(
        "cell_name",
        "Organ",
        "Driver_Gene_Symbol",
        "drivers",
        "Cell_ID_DepMap"
      )
      df[, intersect(cols, names(df)), drop = FALSE]
    })

    output$cell_line_table <- reactable::renderReactable({
      df <- filtered_lines()
      validate(need(nrow(df) > 0, "No cell lines for this organ."))

      col_defs <- list(
        cell_name = reactable::colDef(name = "Cell line", minWidth = 90),
        Organ = reactable::colDef(name = "Organ", minWidth = 80),
        Driver_Gene_Symbol = reactable::colDef(
          name = "Driver gene",
          minWidth = 90
        ),
        drivers = reactable::colDef(name = "Drivers", minWidth = 140)
      )
      if ("Cell_ID_DepMap" %in% names(df)) {
        col_defs[["Cell_ID_DepMap"]] <- reactable::colDef(
          name = "DepMap",
          minWidth = 90,
          html = TRUE,
          cell = function(value) {
            if (is.na(value) || !nzchar(as.character(value))) {
              return("")
            }
            sprintf(
              '<a href="%s%s" target="_blank" rel="noopener">%s</a>',
              .cell_line_depmap_url,
              value,
              value
            )
          }
        )
      }

      tahoe_reactable(df, columns = col_defs)
    })
  })
}
