# Samples & cell-level obs explorer module.
#
# Two sections:
#   1. Samples/plates — always available, backed by the small sample_metadata
#      table. Filter by plate and drug, add parsed dose, chart and export.
#   2. Cell-level obs — a lazy, guarded summary over the huge cell-level obs
#      table via tahoe_obs_summary(), which aggregates in duckdb and never
#      pulls raw cells into R. When the source is remote (HuggingFace) the
#      query is gated behind a "Run query" button so nothing heavy fires on
#      load; for local/fixture sources it computes reactively.

# Group-by choices exposed in the obs section (a friendly subset of the data
# layer's whitelist).
.obs_group_choices <- c(
  "Drug" = "drug",
  "Cell line" = "cell_line",
  "Plate" = "plate",
  "Cell-cycle phase" = "phase",
  "QC filter tier" = "pass_filter"
)

# Metric choices exposed in the obs section.
.obs_metric_choices <- c(
  "Cell count" = "n_cells",
  "Mean % mito" = "mean_pcnt_mito",
  "Mean transcript count" = "mean_tscp_count",
  "Mean gene count" = "mean_gene_count"
)

# Human label for the obs source type.
.obs_source_label <- function(type) {
  switch(
    type,
    local = "local file",
    remote = "remote (HuggingFace, slower)",
    fixture = "demo fixture",
    type
  )
}

# Bootstrap badge colour for the obs source type.
.obs_source_theme <- function(type) {
  switch(
    type,
    local = "text-bg-success",
    remote = "text-bg-warning",
    fixture = "text-bg-secondary",
    "text-bg-secondary"
  )
}

# Horizontal bar of an already-tidy data frame (label + value). Mirrors
# .overview_bar but takes a precomputed frame so awkward source column names
# never reach ggplot.
.obs_bar <- function(plot_df, fill, x_title = "Count") {
  validate(need(nrow(plot_df) > 0, "No data to plot."))
  plot_df$label <- factor(plot_df$label, levels = rev(plot_df$label))
  ggplot2::ggplot(plot_df, ggplot2::aes(x = label, y = value)) +
    ggplot2::geom_col(fill = fill) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      labels = scales::label_comma(),
      expand = ggplot2::expansion(c(0, 0.08))
    ) +
    ggplot2::labs(x = NULL, y = x_title) +
    tahoe_theme()
}

# Histogram of one numeric column, precomputed into a tidy frame.
.obs_hist <- function(values, fill, x_title) {
  plot_df <- data.frame(value = values[is.finite(values)])
  validate(need(nrow(plot_df) > 0, "No data to plot."))
  ggplot2::ggplot(plot_df, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(bins = 20, fill = fill, colour = "white") +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::labs(x = x_title, y = "Samples") +
    tahoe_theme()
}

obs_explorer_ui <- function(id) {
  ns <- NS(id)
  source_type <- tahoe_obs_source()$type

  samples_tab <- bslib::nav_panel(
    title = "Samples & plates",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "Filters",
        selectizeInput(
          ns("sample_plate"),
          "Plate",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "All plates")
        ),
        selectizeInput(
          ns("sample_drug"),
          "Drug",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "All drugs")
        )
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header("Samples per plate"),
          plotly::plotlyOutput(ns("plate_plot"), height = 280)
        ),
        bslib::card(
          bslib::card_header("Samples per drug (top 12)"),
          plotly::plotlyOutput(ns("drug_plot"), height = 280)
        ),
        bslib::card(
          bslib::card_header("Distribution of mean % mito"),
          plotly::plotlyOutput(ns("mito_plot"), height = 260)
        ),
        bslib::card(
          bslib::card_header("Distribution of mean transcript count"),
          plotly::plotlyOutput(ns("tscp_plot"), height = 260)
        )
      ),
      bslib::card(
        bslib::card_header("Filtered samples"),
        reactable::reactableOutput(ns("sample_table")),
        tags$hr(),
        subset_export_ui(ns("sample_export"))
      )
    )
  )

  obs_tab <- bslib::nav_panel(
    title = "Cell-level obs",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "Query",
        tags$span(
          class = paste("badge", .obs_source_theme(source_type)),
          .obs_source_label(source_type)
        ),
        p(
          class = "text-muted small mt-2",
          "Aggregated lazily in duckdb; raw cells are never loaded."
        ),
        selectInput(
          ns("obs_group"),
          "Group by",
          choices = .obs_group_choices
        ),
        selectInput(
          ns("obs_metric"),
          "Metric",
          choices = .obs_metric_choices
        ),
        selectizeInput(
          ns("obs_drug"),
          "Drug filter (optional)",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "All drugs")
        ),
        if (identical(source_type, "remote")) {
          actionButton(
            ns("run"),
            "Run query",
            class = "btn-warning btn-sm",
            icon = icon("play")
          )
        }
      ),
      bslib::card(
        bslib::card_header("Summary"),
        plotly::plotlyOutput(ns("obs_plot"), height = 320)
      ),
      bslib::card(
        bslib::card_header("Summary table"),
        reactable::reactableOutput(ns("obs_table")),
        tags$hr(),
        subset_export_ui(ns("obs_export"))
      )
    )
  )

  tagList(
    div(
      class = "p-2",
      h3("Samples & cells"),
      p(
        class = "text-muted",
        "Filter samples and their plates, then drill into lazily aggregated",
        "cell-level metadata without loading the full cell table."
      )
    ),
    bslib::navset_card_tab(samples_tab, obs_tab)
  )
}

obs_explorer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    source_type <- tahoe_obs_source()$type

    # ---- Samples/plates section ------------------------------------------

    # Full sample table with parsed dose columns attached once.
    samples_all <- reactive({
      df <- tahoe_sample()
      dose <- tahoe_parse_dose(df$drugname_drugconc)
      df$conc <- dose$conc
      df$unit <- dose$unit
      df
    })

    # Populate the filter dropdowns from the loaded data.
    observe({
      df <- samples_all()
      updateSelectizeInput(
        session,
        "sample_plate",
        choices = sort(unique(df$plate)),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "sample_drug",
        choices = sort(unique(df$drug)),
        server = TRUE
      )
    })

    # Samples narrowed by the active plate/drug filters.
    samples_filtered <- reactive({
      df <- samples_all()
      if (length(input$sample_plate) > 0) {
        df <- df[df$plate %in% input$sample_plate, , drop = FALSE]
      }
      if (length(input$sample_drug) > 0) {
        df <- df[df$drug %in% input$sample_drug, , drop = FALSE]
      }
      df
    })

    output$plate_plot <- plotly::renderPlotly({
      df <- samples_filtered()
      counts <- sort(table(df$plate), decreasing = TRUE)
      plot_df <- data.frame(
        label = names(counts),
        value = as.integer(counts),
        stringsAsFactors = FALSE
      )
      tahoe_plotly(.obs_bar(plot_df, tahoe_colors$primary, "Samples"))
    })

    output$drug_plot <- plotly::renderPlotly({
      df <- samples_filtered()
      counts <- utils::head(sort(table(df$drug), decreasing = TRUE), 12)
      plot_df <- data.frame(
        label = names(counts),
        value = as.integer(counts),
        stringsAsFactors = FALSE
      )
      tahoe_plotly(.obs_bar(plot_df, tahoe_colors$green, "Samples"))
    })

    output$mito_plot <- plotly::renderPlotly({
      tahoe_plotly(
        .obs_hist(
          samples_filtered()$mean_pcnt_mito,
          tahoe_colors$blue,
          "Mean % mito"
        )
      )
    })

    output$tscp_plot <- plotly::renderPlotly({
      tahoe_plotly(
        .obs_hist(
          samples_filtered()$mean_tscp_count,
          tahoe_colors$violet,
          "Mean transcript count"
        )
      )
    })

    output$sample_table <- reactable::renderReactable({
      tahoe_reactable(samples_filtered())
    })

    subset_export_server(
      "sample_export",
      data_reactive = samples_filtered,
      file_stem = "samples_subset"
    )

    # ---- Cell-level obs section ------------------------------------------

    # Enumerate drug choices for the obs filter from the obs table itself
    # (grouping by drug returns one row per distinct drug). This is a small,
    # lazy query, so it is safe on local/fixture; for remote we defer it until
    # the section is used by wiring it to the same query trigger below.
    obs_drug_choices <- reactive({
      res <- tahoe_obs_summary("drug", metric = "n_cells", limit = NULL)
      if (!is.null(attr(res, "tahoe_error"))) {
        return(character())
      }
      sort(res$drug)
    })

    # For local/fixture sources populate the drug filter eagerly; for remote,
    # wait until the user first runs a query so nothing heavy fires on load.
    obs_drug_choices_gated <- if (identical(source_type, "remote")) {
      bindEvent(obs_drug_choices, input$run, ignoreNULL = TRUE)
    } else {
      obs_drug_choices
    }

    observe({
      updateSelectizeInput(
        session,
        "obs_drug",
        choices = obs_drug_choices_gated(),
        server = TRUE
      )
    })

    # Compute the aggregated summary. Wrapped so we can gate it behind the
    # explicit "Run query" button when the source is remote.
    obs_query <- function() {
      req(input$obs_group, input$obs_metric)
      filters <- list()
      if (length(input$obs_drug) > 0) {
        filters$drug <- input$obs_drug
      }
      tahoe_obs_summary(
        group_by = input$obs_group,
        filters = filters,
        metric = input$obs_metric,
        limit = 100
      )
    }

    obs_summary <- if (identical(source_type, "remote")) {
      eventReactive(input$run, obs_query(), ignoreNULL = TRUE)
    } else {
      reactive(obs_query())
    }

    # Tidy frame for plotting/tabling: friendly, stable column names and a
    # graceful message when the underlying query failed.
    obs_result <- reactive({
      res <- obs_summary()
      req(res)
      err <- attr(res, "tahoe_error")
      validate(need(
        is.null(err),
        paste0(
          "Could not read the cell-level obs source (",
          .obs_source_label(source_type),
          "). ",
          if (identical(source_type, "remote")) {
            "Check your network connection and try again."
          } else {
            "The data may be unavailable."
          }
        )
      ))
      validate(need(nrow(res) > 0, "No cells match the current selection."))
      res
    })

    # Plot data with the group column renamed to `label` for ggplot.
    obs_plot_df <- reactive({
      res <- obs_result()
      group_col <- names(res)[[1]]
      utils::head(
        data.frame(
          label = as.character(res[[group_col]]),
          value = res$value,
          stringsAsFactors = FALSE
        ),
        20
      )
    })

    output$obs_plot <- plotly::renderPlotly({
      metric_label <- names(.obs_metric_choices)[
        match(input$obs_metric, .obs_metric_choices)
      ]
      tahoe_plotly(.obs_bar(obs_plot_df(), tahoe_colors$sand, metric_label))
    })

    output$obs_table <- reactable::renderReactable({
      tahoe_reactable(obs_result())
    })

    subset_export_server(
      "obs_export",
      data_reactive = obs_result,
      file_stem = "obs_summary"
    )

    # Expose reactives so tests (and callers) can inspect module state.
    list(
      samples_filtered = samples_filtered,
      obs_result = obs_result
    )
  })
}
