# Shared plotting theme and palette for the app ("Alpine Lake").
#
# Modules build ggplots with tahoe_theme() and the tahoe_* colors, then render
# them interactively with tahoe_plotly() so every chart shares one look: clean
# typography, a cohesive Lake-Tahoe palette, hover tooltips, and zoom.

# Palette mirrored from _brand.yml so R plots match the app chrome.
tahoe_colors <- list(
  primary = "#0B7285", # deep lake teal
  green = "#2F9E44", # evergreen
  blue = "#1C7ED6", # sky
  sand = "#E8A317", # sandstone
  orange = "#E8590C",
  violet = "#BE4BDB",
  slate = "#5F6B7A", # granite
  fg = "#212529",
  grid = "#E4E9ED"
)

# Ordered qualitative palette for categorical fills (drugs, organs, ...).
tahoe_categorical <- c(
  "#0B7285",
  "#2F9E44",
  "#1C7ED6",
  "#E8590C",
  "#BE4BDB",
  "#F08C00",
  "#1098AD",
  "#66A80F",
  "#E64980",
  "#5C7CFA"
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
tahoe_theme <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        color = tahoe_colors$grid,
        linewidth = 0.4
      ),
      axis.title = ggplot2::element_text(color = tahoe_colors$slate),
      axis.text = ggplot2::element_text(color = tahoe_colors$slate),
      plot.title = ggplot2::element_text(
        face = "bold",
        color = tahoe_colors$fg
      ),
      legend.position = "none"
    )
}

#' Render a ggplot as a styled, interactive plotly widget. `tooltip` selects
#' which aesthetics appear on hover (defaults to x + y). Chrome is stripped to
#' a small mode bar (hover, zoom, PNG download) with no plotly logo.
tahoe_plotly <- function(p, tooltip = c("x", "y")) {
  plotly::ggplotly(p, tooltip = tooltip) |>
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
        size = 13,
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

# Make the shared theme the default for every ggplot in the app.
ggplot2::theme_set(tahoe_theme())
