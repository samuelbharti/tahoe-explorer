# Shared plotting theme and palette for the app ("Alpine Lake").
#
# Modules build ggplots with tahoe_theme() and the tahoe_* colors, then render
# them interactively with tahoe_plotly() so every chart shares one look: clean
# typography, a cohesive Lake-Tahoe palette, hover tooltips, and zoom.

# Chrome + single-series accents. The accent hues are drawn from the CVD-safe
# categorical palette below so single-series bars stay on-palette; `primary`
# keeps the deep lake teal used across the app chrome (_brand.yml).
tahoe_colors <- list(
  primary = "#0B7285", # deep lake teal (brand chrome)
  blue = "#2A78D6", # lake blue
  green = "#1BAF7A", # aqua-green
  sand = "#EDA100", # sandstone / yellow
  orange = "#EB6834",
  violet = "#4A3AA7",
  slate = "#5F6B7A", # granite
  fg = "#212529",
  grid = "#E4E9ED"
)

# Ordered qualitative palette for categorical fills (drugs, organs, ...).
# Colorblind-safe by construction: the ordering maximizes the minimum adjacent
# CVD separation (worst adjacent ΔE 24.2, well above the ≥12 target under
# protan/deutan/tritan simulation). Do NOT reorder without re-validating with
# the dataviz palette validator — the ordering IS the safety mechanism. Leads
# with lake blue / aqua / evergreen, so it also reads as "Tahoe".
tahoe_categorical <- c(
  "#2A78D6", # blue
  "#1BAF7A", # aqua
  "#EDA100", # yellow
  "#008300", # green
  "#4A3AA7", # violet
  "#E34948", # red
  "#E87BA4", # magenta
  "#EB6834" # orange
)

#' n distinct categorical colors, interpolating when n exceeds the base set.
tahoe_pal <- function(n) {
  if (n <= length(tahoe_categorical)) {
    return(tahoe_categorical[seq_len(n)])
  }
  grDevices::colorRampPalette(tahoe_categorical)(n)
}

#' Shared ggplot2 theme: minimal, Inter-ish, restrained gridlines. Fonts are
#' applied in the browser by tahoe_plotly(), so no base_family is set here
#' (avoids R font-registration warnings).
tahoe_theme <- function(base_size = 14) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        color = tahoe_colors$grid,
        linewidth = 0.4
      ),
      axis.title = ggplot2::element_text(
        color = tahoe_colors$slate,
        size = ggplot2::rel(0.9),
        face = "bold"
      ),
      axis.text = ggplot2::element_text(
        color = tahoe_colors$slate,
        size = ggplot2::rel(0.9)
      ),
      plot.title = ggplot2::element_text(
        face = "bold",
        color = tahoe_colors$fg,
        size = ggplot2::rel(1.05)
      ),
      plot.margin = ggplot2::margin(6, 10, 6, 6),
      legend.position = "none"
    )
}

#' Render a ggplot as a styled, interactive plotly widget. `tooltip` selects
#' which aesthetics appear on hover (defaults to x + y). Chrome is stripped to
#' a small mode bar (hover, zoom, PNG download) with no plotly logo. Pass a
#' `source` id to make the chart emit click events readable in a Shiny server
#' with `plotly::event_data("plotly_click", source = <id>)` — pair it with a
#' `key` aesthetic on the marks so the click carries the clicked category.
tahoe_plotly <- function(p, tooltip = c("x", "y"), source = NULL) {
  src <- if (is.null(source)) "A" else source
  widget <- plotly::ggplotly(p, tooltip = tooltip, source = src)
  if (!is.null(source)) {
    widget <- plotly::event_register(widget, "plotly_click")
  }
  widget |>
    plotly::config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = list(
        "lasso2d",
        "select2d",
        "autoScale2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian",
        "toggleSpikelines",
        "zoomIn2d",
        "zoomOut2d"
      )
    ) |>
    plotly::layout(
      font = list(
        family = "Inter, system-ui, sans-serif",
        size = 14,
        color = tahoe_colors$fg
      ),
      hoverlabel = list(
        font = list(family = "Inter, system-ui, sans-serif"),
        bgcolor = "white"
      ),
      margin = list(l = 8, r = 8, b = 8, t = 12),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    )
}

#' Alpine Lake reactable theme — shared look for every table in the app: Inter
#' type, teal highlights, restrained hairline borders, tabular figures.
tahoe_reactable_theme <- function() {
  reactable::reactableTheme(
    color = tahoe_colors$fg,
    borderColor = tahoe_colors$grid,
    highlightColor = "#EAF3F5", # pale teal wash
    cellPadding = "8px 10px",
    style = list(
      fontFamily = "Inter, system-ui, sans-serif",
      fontSize = "14px"
    ),
    headerStyle = list(
      color = tahoe_colors$slate,
      fontWeight = 600,
      borderBottom = paste0("2px solid ", tahoe_colors$grid)
    ),
    searchInputStyle = list(width = "100%")
  )
}

#' A reactable with the app's shared, UI-friendly defaults: searchable,
#' sortable, compact, row highlight, paginated, tabular figures, Alpine theme.
#' Pass through any extra reactable() arguments (e.g. `columns`, `onClick`).
tahoe_reactable <- function(data, ..., page_size = 10) {
  reactable::reactable(
    data,
    searchable = TRUE,
    sortable = TRUE,
    striped = TRUE,
    highlight = TRUE,
    compact = TRUE,
    borderless = FALSE,
    defaultPageSize = page_size,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(10, 25, 50),
    theme = tahoe_reactable_theme(),
    ...
  )
}

# Make the shared theme the default for every ggplot in the app.
ggplot2::theme_set(tahoe_theme())
