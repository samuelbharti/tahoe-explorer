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
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0, 0.12))) +
    ggplot2::labs(x = NULL, y = "Count") +
    tahoe_theme()
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
        plotly::plotlyOutput(ns("organ_plot"), height = 320)
      ),
      bslib::card(
        bslib::card_header("Top driver genes"),
        plotly::plotlyOutput(ns("gene_plot"), height = 320)
      )
    ),
    bslib::card(
      bslib::card_header("Variant-type breakdown"),
      plotly::plotlyOutput(ns("var_type_plot"), height = 300)
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        span("Somatic variants"),
        .info_pop(
          paste(
            "Somatic mutation calls for the matching cell lines. Full somatic",
            "profiles from DepMap 24Q4 (CC BY 4.0) where available, with curated",
            "driver variants from Cellosaurus for lines DepMap does not cover.",
            "One row per variant; the source column shows the origin. Empty",
            "until dev/download_variants.R is run; the demo uses synthetic data."
          ),
          title = "Somatic variants"
        )
      ),
      reactable::reactableOutput(ns("variants"))
    ),
    bslib::card(
      bslib::card_header("Most frequently mutated genes"),
      plotly::plotlyOutput(ns("mutated_genes_plot"), height = 300)
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

    # Collapse the matched driver-level rows to one row per cell line (with a
    # drivers summary) for the table, organ chart, and export, so "cell lines"
    # counts distinct lines rather than the driver-level rows of the source
    # table. The driver-gene and variant-type charts keep the driver-level
    # `filtered` set below.
    filtered_lines <- reactive({
      names_matched <- unique(filtered()$cell_name)
      unique_lines <- tahoe_cell_line_unique()
      unique_lines[
        unique_lines$cell_name %in% names_matched,
        ,
        drop = FALSE
      ]
    })

    output$table <- reactable::renderReactable({
      df <- filtered_lines()
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

      tahoe_reactable(df, columns = col_defs)
    })

    # Somatic variants for the matching lines: an inner join of the external
    # variant table onto the filtered cell lines by cell name (every line has
    # one, so DepMap- and Cellosaurus-sourced rows both attach). Empty (not an
    # error) when the variant table is absent or nothing matches.
    variants <- reactive({
      v <- tryCatch(tahoe_cell_variants(), error = function(e) NULL)
      lines <- filtered_lines()
      if (
        is.null(v) ||
          nrow(v) == 0 ||
          !"cell_name" %in% names(v) ||
          !"cell_name" %in% names(lines)
      ) {
        return(dplyr::tibble(cell_name = character(), gene = character()))
      }
      keep <- intersect(c("cell_name", "Organ"), names(lines))
      dplyr::inner_join(lines[, keep, drop = FALSE], v, by = "cell_name")
    })

    output$variants <- reactable::renderReactable({
      v <- variants()
      validate(need(
        nrow(v) > 0,
        paste(
          "No somatic variants for the matching cell lines.",
          "Run dev/download_variants.R to load DepMap / Cellosaurus variants",
          "(the offline demo shows synthetic variants)."
        )
      ))
      pref <- c(
        "cell_name",
        "source",
        "gene",
        "protein_change",
        "variant_type",
        "consequence",
        "zygosity",
        "hotspot",
        "likely_lof",
        "dbsnp"
      )
      tahoe_reactable(v[, intersect(pref, names(v)), drop = FALSE])
    })

    output$mutated_genes_plot <- plotly::renderPlotly({
      v <- variants()
      validate(need(
        nrow(v) > 0 && "gene" %in% names(v),
        "No variant data to plot."
      ))
      tahoe_plotly(.cell_line_bar(v, "gene", tahoe_colors$orange))
    })

    output$organ_plot <- plotly::renderPlotly({
      df <- filtered_lines()
      validate(need(nrow(df) > 0, "No cell lines match the current filters."))
      tahoe_plotly(.cell_line_bar(df, "Organ", tahoe_colors$green))
    })

    output$gene_plot <- plotly::renderPlotly({
      df <- filtered()
      validate(need(nrow(df) > 0, "No cell lines match the current filters."))
      tahoe_plotly(.cell_line_bar(df, "Driver_Gene_Symbol", tahoe_colors$blue))
    })

    output$var_type_plot <- plotly::renderPlotly({
      df <- filtered()
      validate(need(nrow(df) > 0, "No cell lines match the current filters."))
      tahoe_plotly(.cell_line_bar(df, "Driver_VarType", tahoe_colors$orange))
    })

    subset_export_server(
      "export",
      data_reactive = filtered_lines,
      file_stem = "cell_lines_subset"
    )

    # Expose the collapsed one-row-per-cell-line reactive for callers; the
    # driver-level `filtered` remains available in-scope for tests.
    filtered_lines
  })
}
