# Coverage module.
#
# The "can I run this experiment?" view: an interactive drug x cell-line matrix
# colored by how many cells were profiled for each combination, with a per-cell
# dose breakdown on click. Tahoe-100M is fully crossed (every drug in every
# cell line), so the signal here is cell depth and dose completeness, not
# presence/absence. All from the local cell grid, so it is fast.

# Sequential blue ramp for cell depth (light = few cells, dark = many).
.coverage_fill_low <- "#CDE2FB"
.coverage_fill_high <- "#0D366B"

coverage_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      title = "Matrix",
      selectizeInput(
        ns("drugs"),
        "Drugs (rows)",
        choices = NULL,
        multiple = TRUE,
        options = list(placeholder = "Search drugs…")
      ),
      selectizeInput(
        ns("organs"),
        "Organs (columns)",
        choices = NULL,
        multiple = TRUE,
        options = list(placeholder = "All organs")
      ),
      div(
        class = "text-muted small",
        "Color = cells profiled (log scale). Click a tile for its dose",
        "breakdown."
      ),
      actionLink(ns("reset"), "Reset to top drugs", class = "small")
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        span("Cells profiled — drug × cell line"),
        .info_pop(
          paste(
            "Each tile is one drug tested in one cell line; color shows how",
            "many cells were profiled (log scale). Tahoe-100M is fully",
            "crossed, so darker = more statistical power for that combination.",
            "Click a tile to see its per-dose cell counts."
          ),
          title = "Coverage matrix"
        )
      ),
      plotly::plotlyOutput(ns("heatmap"), height = "56vh")
    ),
    bslib::card(
      height = "34vh",
      bslib::card_header(uiOutput(ns("detail_title"), inline = TRUE)),
      plotly::plotlyOutput(ns("detail_plot"), height = "24vh")
    )
  )
}

# Row (drug) and column (cell line) orderings for the matrix. Cell lines are
# grouped by organ; drugs sorted by total cells. Shared by the plot and the
# click handler, which maps plotly's numeric axis indices back to labels.
.coverage_orders <- function(df) {
  list(
    line = unique(df$cell_name[order(df$organ, df$cell_name)]),
    drug = names(sort(tapply(df$n_cells, df$drug, sum)))
  )
}

# Heatmap of cells profiled per (drug x cell line).
.coverage_heatmap <- function(df, line_order = NULL, drug_order = NULL) {
  validate(need(nrow(df) > 0, "No drug × cell-line combinations selected."))
  if (is.null(line_order) || is.null(drug_order)) {
    ord <- .coverage_orders(df)
    line_order <- ord$line
    drug_order <- ord$drug
  }
  df$cell_name <- factor(df$cell_name, levels = line_order)
  df$drug <- factor(df$drug, levels = drug_order)
  df$text <- paste0(
    df$drug,
    " × ",
    df$cell_name,
    "\n",
    format(df$n_cells, big.mark = ","),
    " cells",
    ifelse(nzchar(df$doses), paste0(" · doses ", df$doses, " µM"), "")
  )
  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = cell_name,
      y = drug,
      fill = n_cells,
      text = text
    )
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.4) +
    ggplot2::scale_fill_gradient(
      low = .coverage_fill_low,
      high = .coverage_fill_high,
      trans = "log10",
      labels = scales::label_number(scale_cut = scales::cut_short_scale()),
      name = "Cells"
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    tahoe_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right"
    )
}

# Per-dose cell counts for one (drug x cell line), summed over plates.
.coverage_dose_bar <- function(rows) {
  validate(need(nrow(rows) > 0, "No cells for this combination."))
  agg <- dplyr::summarise(
    dplyr::group_by(rows, conc),
    n_cells = sum(n_cells),
    n_plates = dplyr::n_distinct(plate),
    .groups = "drop"
  )
  agg$label <- ifelse(agg$conc == 0, "Control (DMSO)", paste0(agg$conc, " µM"))
  agg$label <- factor(agg$label, levels = agg$label[order(agg$conc)])
  agg$text <- paste0(
    agg$label,
    ": ",
    format(agg$n_cells, big.mark = ","),
    " cells across ",
    agg$n_plates,
    " plate(s)"
  )
  ggplot2::ggplot(
    agg,
    ggplot2::aes(x = label, y = n_cells, text = text)
  ) +
    ggplot2::geom_col(fill = tahoe_colors$primary, width = 0.7) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(scale_cut = scales::cut_short_scale()),
      expand = ggplot2::expansion(c(0, 0.12))
    ) +
    ggplot2::labs(x = NULL, y = "Cells") +
    tahoe_theme()
}

coverage_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    click_src <- session$ns("cov")
    coverage <- reactive(tahoe_coverage())

    # Default drug selection: the top drugs by total cells profiled.
    top_drugs <- reactive({
      cov <- coverage()
      totals <- sort(tapply(cov$n_cells, cov$drug, sum), decreasing = TRUE)
      utils::head(names(totals), 30)
    })

    observeEvent(coverage(), once = TRUE, {
      cov <- coverage()
      updateSelectizeInput(
        session,
        "drugs",
        choices = sort(unique(cov$drug)),
        selected = top_drugs(),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "organs",
        choices = sort(unique(cov$organ)),
        selected = character(0),
        server = TRUE
      )
    })

    observeEvent(input$reset, {
      updateSelectizeInput(session, "drugs", selected = top_drugs())
      updateSelectizeInput(session, "organs", selected = character(0))
    })

    filtered <- reactive({
      cov <- coverage()
      if (!is.null(input$drugs) && length(input$drugs) > 0) {
        cov <- cov[cov$drug %in% input$drugs, , drop = FALSE]
      }
      if (!is.null(input$organs) && length(input$organs) > 0) {
        cov <- cov[cov$organ %in% input$organs, , drop = FALSE]
      }
      cov
    })

    orders <- reactive(.coverage_orders(filtered()))

    output$heatmap <- plotly::renderPlotly({
      o <- orders()
      tahoe_plotly(
        .coverage_heatmap(filtered(), o$line, o$drug),
        tooltip = "text",
        source = click_src
      )
    })

    # geom_tile renders as a single plotly heatmap trace, so the click reports
    # numeric axis indices (x = column, y = row); map them back to labels using
    # the same orderings the plot was drawn with.
    clicked <- reactiveVal(NULL)
    observeEvent(plotly::event_data("plotly_click", source = click_src), {
      d <- plotly::event_data("plotly_click", source = click_src)
      if (is.null(d$x) || is.null(d$y)) {
        return()
      }
      o <- orders()
      xi <- round(as.numeric(d$x))
      yi <- round(as.numeric(d$y))
      if (
        is.na(xi) ||
          is.na(yi) ||
          xi < 1 ||
          yi < 1 ||
          xi > length(o$line) ||
          yi > length(o$drug)
      ) {
        return()
      }
      clicked(c(
        drug = as.character(o$drug[[yi]]),
        cell_name = as.character(o$line[[xi]])
      ))
    })

    output$detail_title <- renderUI({
      cl <- clicked()
      if (is.null(cl)) {
        span("Dose breakdown — click a tile")
      } else {
        tagList(
          "Dose breakdown — ",
          tags$strong(cl[[1]]),
          " × ",
          tags$strong(cl[[2]])
        )
      }
    })

    output$detail_plot <- plotly::renderPlotly({
      cl <- clicked()
      validate(need(
        !is.null(cl),
        "Click a tile in the matrix to see its per-dose cell counts."
      ))
      g <- tahoe_cell_grid()
      rows <- g[g$drug == cl[[1]] & g$cell_name == cl[[2]], , drop = FALSE]
      tahoe_plotly(.coverage_dose_bar(rows), tooltip = "text")
    })
  })
}
