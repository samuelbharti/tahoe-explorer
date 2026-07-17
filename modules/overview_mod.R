# Overview module.
#
# Landing tab: headline dataset counts as value boxes, plus an interactive,
# color-coded "cell lines by organ" chart linked to a cell-line metadata table.
# Click an organ bar to filter the table (and dim the other organs); click a
# table row to break that cell line's drivers down into two plots below. Reads
# only the small metadata tables, so it is fast.

# A small, unobtrusive info affordance: an info icon in a card header that
# reveals a short explanation on click (context without blocking the workflow).
.info_pop <- function(msg, title = NULL) {
  bslib::popover(
    shiny::icon(
      "circle-info",
      class = "text-muted ms-2",
      style = "cursor: pointer;"
    ),
    msg,
    title = title,
    placement = "auto"
  )
}

# Oncogene vs tumor-suppressor get fixed, meaningful colors (red / blue).
.gene_type_colors <- c(
  Oncogene = "#E34948",
  Suppressor = "#2A78D6",
  Unknown = "#5F6B7A"
)

overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("summary_boxes")),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        height = "44vh",
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Cell lines by organ"),
          .info_pop(
            paste(
              "Each bar counts the assayed cell lines from one organ system",
              "(the tissue the line was derived from). Colors identify organs",
              "and match the table. Click a bar to filter the table to that",
              "organ; click it again to clear."
            ),
            title = "Cell lines by organ"
          )
        ),
        plotly::plotlyOutput(ns("organ_plot"), height = "37vh"),
        bslib::card_footer(
          class = "text-muted small",
          "Click a bar to filter · click it again to clear."
        )
      ),
      bslib::card(
        height = "44vh",
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          div(
            class = "d-flex align-items-center",
            uiOutput(ns("table_title"), inline = TRUE),
            .info_pop(
              paste(
                "The 50 cell lines assayed in Tahoe-100M, with their oncogenic",
                "driver mutations. Click a row to break that line's drivers",
                "down below. DepMap links open the external cell-line portal."
              ),
              title = "Cell lines"
            )
          ),
          uiOutput(ns("clear_filter"), inline = TRUE)
        ),
        tahoe_table_ui(ns("cell_line_table")),
        bslib::card_footer(
          class = "text-muted small",
          "Click a row to see its driver profile below."
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        height = 360,
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Driver genes"),
          .info_pop(
            paste(
              "Oncogenic driver genes annotated for the selected cell line,",
              "colored by role: oncogene (activating) vs tumor suppressor",
              "(loss-of-function)."
            ),
            title = "Driver genes"
          )
        ),
        plotly::plotlyOutput(ns("driver_gene_plot"), height = 260)
      ),
      bslib::card(
        height = 360,
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Variant classes"),
          .info_pop(
            paste(
              "How the selected cell line's driver mutations break down by",
              "variant class — missense, deletion, frameshift, and so on."
            ),
            title = "Variant classes"
          )
        ),
        plotly::plotlyOutput(ns("variant_plot"), height = 260)
      )
    )
  )
}

# Stable organ -> color map, assigned in descending frequency order so the most
# common organs get the leading (most distinct) palette hues.
.overview_organ_colors <- function(organs) {
  ord <- names(sort(table(organs), decreasing = TRUE))
  stats::setNames(tahoe_pal(length(ord)), ord)
}

# Color-coded organ bar chart. `key = label` carries the organ name for click
# events; `selected` dims every other organ so the picked one stands out.
.overview_organ_bar <- function(df, colors, selected = NULL, top_n = 15) {
  validate(need("Organ" %in% names(df), "Organ column not available"))
  counts <- sort(table(df[["Organ"]]), decreasing = TRUE)
  counts <- utils::head(counts, top_n)
  plot_df <- data.frame(
    label = factor(names(counts), levels = rev(names(counts))),
    n = as.integer(counts)
  )
  plot_df$sel <- if (is.null(selected)) {
    TRUE
  } else {
    as.character(plot_df$label) == selected
  }
  plot_df$text <- paste0(plot_df$label, ": ", plot_df$n, " cell lines")
  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = label,
      y = n,
      fill = label,
      alpha = sel,
      key = label,
      text = text
    )
  ) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = colors, guide = "none") +
    ggplot2::scale_alpha_manual(
      values = c(`TRUE` = 1, `FALSE` = 0.22),
      guide = "none"
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0, 0.12))) +
    ggplot2::labs(x = NULL, y = "Cell lines") +
    tahoe_theme()
}

# Driver genes of one cell line, one bar per gene, colored by gene role.
.overview_driver_gene_bar <- function(rows) {
  rows <- rows[!is.na(rows$Driver_Gene_Symbol), , drop = FALSE]
  validate(need(nrow(rows) > 0, "No driver genes annotated for this line."))
  rows$role <- ifelse(
    is.na(rows$Driver_GeneType_DM),
    "Unknown",
    rows$Driver_GeneType_DM
  )
  genes <- unique(rows$Driver_Gene_Symbol)
  rows$gene <- factor(rows$Driver_Gene_Symbol, levels = rev(genes))
  rows$text <- paste0(
    rows$Driver_Gene_Symbol,
    " · ",
    rows$Driver_VarType,
    " · ",
    rows$role
  )
  ggplot2::ggplot(
    rows,
    ggplot2::aes(x = gene, y = 1, fill = role, text = text)
  ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = .gene_type_colors, name = NULL) +
    ggplot2::labs(x = NULL, y = NULL) +
    tahoe_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

# Variant-class breakdown of one cell line's drivers, using a stable class color.
.overview_variant_bar <- function(rows, colors) {
  rows <- rows[!is.na(rows$Driver_VarType), , drop = FALSE]
  validate(need(nrow(rows) > 0, "No variant classes annotated for this line."))
  counts <- sort(table(rows$Driver_VarType), decreasing = TRUE)
  plot_df <- data.frame(
    label = factor(names(counts), levels = rev(names(counts))),
    n = as.integer(counts)
  )
  plot_df$text <- paste0(plot_df$label, ": ", plot_df$n)
  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = label, y = n, fill = label, text = text)
  ) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = colors, guide = "none") +
    ggplot2::scale_y_continuous(
      breaks = scales::breaks_pretty(),
      expand = ggplot2::expansion(c(0, 0.12))
    ) +
    ggplot2::labs(x = NULL, y = "Drivers") +
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

    # Stable color maps shared by the chart and table.
    organ_colors <- reactive(.overview_organ_colors(lines()$Organ))
    variant_colors <- reactive({
      classes <- sort(unique(stats::na.omit(tahoe_cell_line()$Driver_VarType)))
      stats::setNames(tahoe_pal(length(classes)), classes)
    })

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
      vb <- function(title, value, theme) {
        bslib::value_box(title, value, theme = theme, height = "96px")
      }
      tagList(
        bslib::layout_columns(
          fill = FALSE,
          vb("Cells", fmt_big(cc$cells), "primary"),
          vb("Cell lines", fmt(cc$cell_lines), "primary"),
          vb("Drugs", fmt(cc$drugs), "primary"),
          vb("Samples", fmt(cc$samples), "secondary"),
          vb("Plates", fmt(cc$plates), "secondary"),
          vb("Genes", fmt(cc$genes), "secondary")
        ),
        p(class = "text-muted small mt-2", source_note)
      )
    })

    output$organ_plot <- plotly::renderPlotly({
      tahoe_plotly(
        .overview_organ_bar(lines(), organ_colors(), selected_organ()),
        tooltip = "text",
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

    # Per-column overrides: organ swatch + DepMap link. Reads organ_colors()
    # (a reactive), so the table re-colors when the palette changes.
    cell_line_cols <- function(df) {
      oc <- organ_colors()
      col_defs <- list(
        Organ = reactable::colDef(
          html = TRUE,
          cell = function(value) {
            color <- oc[[value]]
            if (is.null(color)) {
              color <- tahoe_colors$slate
            }
            sprintf(
              paste0(
                '<span style="display:inline-flex;align-items:center;',
                'gap:6px;"><span style="width:.6rem;height:.6rem;',
                'border-radius:50%%;background:%s;flex:none;"></span>%s</span>'
              ),
              color,
              value
            )
          }
        ),
        drivers = reactable::colDef(minWidth = 150)
      )
      if ("Cell_ID_DepMap" %in% names(df)) {
        col_defs[["Cell_ID_DepMap"]] <- reactable::colDef(
          name = "DepMap",
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
      col_defs
    }

    ovtbl <- tahoe_table_server(
      "cell_line_table",
      data = filtered_lines,
      columns = cell_line_cols,
      selection = "single",
      on_click = "select",
      pagination = FALSE,
      height = "30vh",
      empty_message = "No cell lines for this organ."
    )

    # The cell line clicked in the table (maps the selected row to its name).
    selected_cell <- reactive({
      idx <- ovtbl$selected()
      fl <- filtered_lines()
      if (is.null(idx) || length(idx) == 0 || idx > nrow(fl)) {
        return(NULL)
      }
      fl$cell_name[[idx]]
    })

    driver_rows <- reactive({
      cn <- selected_cell()
      if (is.null(cn)) {
        return(NULL)
      }
      cl <- tahoe_cell_line()
      cl[!is.na(cl$cell_name) & cl$cell_name == cn, , drop = FALSE]
    })

    output$driver_gene_plot <- plotly::renderPlotly({
      rows <- driver_rows()
      validate(need(
        !is.null(rows),
        "Click a cell line in the table to see its driver genes."
      ))
      tahoe_plotly(.overview_driver_gene_bar(rows), tooltip = "text")
    })

    output$variant_plot <- plotly::renderPlotly({
      rows <- driver_rows()
      validate(need(
        !is.null(rows),
        "Click a cell line to see its variant classes."
      ))
      tahoe_plotly(
        .overview_variant_bar(rows, variant_colors()),
        tooltip = "text"
      )
    })
  })
}
