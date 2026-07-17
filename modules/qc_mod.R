# QC guardrails module.
#
# Design/rigor checks to run before committing compute: which conditions are
# underpowered (too few cells), which drug x cell-line combos lack the full
# dose series, how the vehicle control is covered, and how drugs/cell lines map
# onto plates (batch structure). All from the local cell grid.

qc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # tour_* ids anchor the guided demo (see R/tour.R).
    div(id = ns("tour_boxes"), uiOutput(ns("value_boxes"))),
    bslib::card(
      id = ns("tour_controls"),
      bslib::card_body(
        class = "py-2",
        bslib::layout_columns(
          col_widths = c(8, 4),
          sliderInput(
            ns("min_cells"),
            "Minimum cells per condition (drug × cell line × dose) to consider well-powered",
            min = 0,
            max = 1000,
            value = 100,
            step = 50,
            width = "100%"
          ),
          div(
            class = "d-flex align-items-end h-100 pb-2",
            checkboxInput(
              ns("qc_only"),
              "Count only QC-passing cells (pass_filter = full)",
              value = FALSE
            )
          )
        )
      )
    ),
    bslib::layout_columns(
      id = ns("tour_power"),
      col_widths = c(6, 6),
      bslib::card(
        height = "44vh",
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Underpowered conditions"),
          div(
            class = "d-flex gap-2 align-items-center",
            tahoe_table_columns_ui(ns("under_table")),
            .info_pop(
              paste(
                "Treatment conditions (drug × cell line × dose) with fewer cells",
                "than the threshold above — the smallest are most at risk of",
                "unstable differential-expression estimates. Raise the threshold",
                "to be more conservative."
              ),
              title = "Underpowered conditions"
            )
          )
        ),
        tahoe_table_ui(ns("under_table"))
      ),
      bslib::card(
        height = "44vh",
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Incomplete dose series"),
          div(
            class = "d-flex gap-2 align-items-center",
            tahoe_table_columns_ui(ns("dose_table")),
            .info_pop(
              paste(
                "Drug × cell-line combinations missing one or more of the three",
                "doses (0.05 / 0.5 / 5 µM) — dose-response analyses for these are",
                "limited."
              ),
              title = "Incomplete dose series"
            )
          )
        ),
        tahoe_table_ui(ns("dose_table"))
      )
    ),
    bslib::layout_columns(
      id = ns("tour_batch"),
      col_widths = c(6, 6),
      bslib::card(
        height = "40vh",
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Vehicle control coverage (DMSO per cell line)"),
          .info_pop(
            paste(
              "Cells of the DMSO_TF vehicle control per cell line. Robust",
              "controls anchor every drug comparison; a cell line with few",
              "control cells weakens all its contrasts."
            ),
            title = "Control coverage"
          )
        ),
        plotly::plotlyOutput(ns("control_plot"), height = "30vh")
      ),
      bslib::card(
        height = "40vh",
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Plate / batch structure"),
          .info_pop(
            paste(
              "How cell lines and drugs map onto the plates. If a drug sits",
              "on only a few plates, its signal is confounded with those",
              "plates — compare it against DMSO controls from the same plates."
            ),
            title = "Plate / batch structure"
          )
        ),
        uiOutput(ns("plate_note")),
        plotly::plotlyOutput(ns("plate_plot"), height = "22vh")
      )
    ),
    bslib::card(
      id = ns("tour_phase"),
      height = "42vh",
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        span("Cell-cycle phase composition"),
        .info_pop(
          paste(
            "Share of profiled cells in each cell-cycle phase (G1 / S / G2M),",
            "by organ. A tissue or drug skewed toward S/G2M (cycling) vs G1",
            "(arrested) is a phenotype worth accounting for before",
            "differential-expression analysis. From the cell grid's phase",
            "counts."
          ),
          title = "Cell-cycle composition"
        )
      ),
      plotly::plotlyOutput(ns("phase_plot"), height = "32vh")
    )
  )
}

# Stacked proportion bar of cell-cycle phase (G1/S/G2M) per organ.
.qc_phase_bar <- function(df) {
  validate(need(
    nrow(df) > 0,
    "Cell-cycle phase data is not available in this grid."
  ))
  long <- dplyr::tibble(
    organ = rep(df$organ, 3),
    phase = rep(c("G1", "S", "G2M"), each = nrow(df)),
    n = c(df$G1, df$S, df$G2M)
  )
  long$phase <- factor(long$phase, levels = c("G1", "S", "G2M"))
  totals <- tapply(long$n, long$organ, sum)
  long$organ <- factor(long$organ, levels = names(sort(totals)))
  long$text <- paste0(
    long$organ,
    " — ",
    long$phase,
    ": ",
    format(long$n, big.mark = ","),
    " cells"
  )
  ggplot2::ggplot(
    long,
    ggplot2::aes(x = organ, y = n, fill = phase, text = text)
  ) +
    ggplot2::geom_col(position = "fill", width = 0.8) +
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::scale_fill_manual(
      values = c(
        G1 = tahoe_colors$blue,
        S = tahoe_colors$primary,
        G2M = tahoe_colors$green
      ),
      name = NULL
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Share of cells") +
    tahoe_theme()
}

# Bar of DMSO control cells per cell line (ascending, so the weakest are first).
.qc_control_bar <- function(df) {
  validate(need(nrow(df) > 0, "No control (DMSO) cells found."))
  df <- df[order(df$n_cells), , drop = FALSE]
  df$cell_name <- factor(df$cell_name, levels = df$cell_name)
  df$text <- paste0(df$cell_name, ": ", format(df$n_cells, big.mark = ","))
  ggplot2::ggplot(
    df,
    ggplot2::aes(x = cell_name, y = n_cells, text = text)
  ) +
    ggplot2::geom_col(fill = tahoe_colors$primary, width = 0.8) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(scale_cut = scales::cut_short_scale()),
      expand = ggplot2::expansion(c(0, 0.12))
    ) +
    ggplot2::labs(x = NULL, y = "DMSO cells") +
    tahoe_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank())
}

# Distribution of how many plates each (non-control) drug appears on.
.qc_plate_bar <- function(df) {
  validate(need(nrow(df) > 0, "No drug/plate data."))
  tab <- as.data.frame(table(plates = df$n_plates), stringsAsFactors = FALSE)
  tab$plates <- factor(tab$plates, levels = sort(as.integer(tab$plates)))
  tab$text <- paste0(tab$Freq, " drugs on ", tab$plates, " plate(s)")
  ggplot2::ggplot(
    tab,
    ggplot2::aes(x = plates, y = Freq, text = text)
  ) +
    ggplot2::geom_col(fill = tahoe_colors$blue, width = 0.8) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0, 0.12))) +
    ggplot2::labs(x = "Plates a drug appears on", y = "Drugs") +
    tahoe_theme()
}

qc_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    conditions <- reactive(tahoe_conditions())
    coverage <- reactive(tahoe_coverage())

    # Treatment conditions only (drop the dose-0 vehicle control).
    treatments <- reactive({
      cond <- conditions()
      cond[cond$conc > 0, , drop = FALSE]
    })

    # Which cell count defines "power": all cells, or only QC-passing (full)
    # cells when the toggle is on and the grid carries the QC-tier column.
    power_col <- reactive({
      if (isTRUE(input$qc_only) && "n_full" %in% names(conditions())) {
        "n_full"
      } else {
        "n_cells"
      }
    })

    underpowered <- reactive({
      thr <- input$min_cells %||% 100
      tr <- treatments()
      pc <- power_col()
      tr[tr[[pc]] < thr, , drop = FALSE]
    })

    incomplete <- reactive({
      cov <- coverage()
      cov[cov$drug != "DMSO_TF" & cov$n_doses < 3, , drop = FALSE]
    })

    control_by_line <- reactive({
      cond <- conditions()
      ctrl <- cond[cond$drug == "DMSO_TF", , drop = FALSE]
      dplyr::summarise(
        dplyr::group_by(ctrl, cell_name),
        n_cells = sum(n_cells),
        .groups = "drop"
      )
    })

    plates_per_drug <- reactive({
      g <- tahoe_cell_grid()
      d <- g[g$drug != "DMSO_TF", , drop = FALSE]
      dplyr::summarise(
        dplyr::group_by(d, drug),
        n_plates = dplyr::n_distinct(plate),
        .groups = "drop"
      )
    })

    # Cell-cycle phase counts per organ, from the grid's phase columns (present
    # only when the grid is the extended build). Empty otherwise -> the plot
    # shows an explanatory message rather than erroring.
    phase_by_organ <- reactive({
      g <- tahoe_cell_grid()
      if (!all(c("n_g1", "n_s", "n_g2m", "organ") %in% names(g))) {
        return(dplyr::tibble(
          organ = character(),
          G1 = numeric(),
          S = numeric(),
          G2M = numeric()
        ))
      }
      d <- g[!is.na(g$organ), , drop = FALSE]
      dplyr::summarise(
        dplyr::group_by(d, organ),
        G1 = sum(n_g1),
        S = sum(n_s),
        G2M = sum(n_g2m),
        .groups = "drop"
      )
    })

    output$value_boxes <- renderUI({
      thr <- input$min_cells %||% 100
      n_under <- nrow(underpowered())
      n_total <- nrow(treatments())
      pct <- if (n_total > 0) round(100 * n_under / n_total) else 0
      n_incomplete <- nrow(incomplete())
      ctrl <- control_by_line()
      min_ctrl <- if (nrow(ctrl) > 0) min(ctrl$n_cells) else NA_real_
      cond <- conditions()
      qc_rate <- if ("n_full" %in% names(cond) && sum(cond$n_cells) > 0) {
        sum(cond$n_full) / sum(cond$n_cells)
      } else {
        NA_real_
      }
      cell_word <- if (identical(power_col(), "n_full")) {
        "QC-passing cells"
      } else {
        "cells"
      }
      fmt <- function(x) {
        if (is.na(x)) "—" else format(x, big.mark = ",")
      }
      boxes <- list(
        bslib::value_box(
          "Underpowered conditions",
          fmt(n_under),
          paste0(pct, "% of treatments < ", fmt(thr), " ", cell_word),
          theme = if (pct > 25) "warning" else "primary",
          height = "120px"
        ),
        bslib::value_box(
          "Incomplete dose series",
          fmt(n_incomplete),
          "drug × cell-line combos missing a dose",
          theme = if (n_incomplete > 0) "warning" else "success",
          height = "120px"
        ),
        bslib::value_box(
          "Weakest control",
          fmt(min_ctrl),
          "fewest DMSO cells in any cell line",
          theme = "secondary",
          height = "120px"
        )
      )
      if (!is.na(qc_rate)) {
        boxes <- c(
          boxes,
          list(bslib::value_box(
            "QC pass rate",
            paste0(round(100 * qc_rate), "%"),
            "cells passing the full QC filter",
            theme = "secondary",
            height = "120px"
          ))
        )
      }
      do.call(bslib::layout_columns, c(list(fill = FALSE), boxes))
    })

    # Ordered, labelled view of the underpowered conditions for display.
    under_display <- reactive({
      df <- underpowered()
      if (is.null(df) || nrow(df) == 0) {
        return(df)
      }
      has_full <- "n_full" %in% names(df)
      cols <- c(
        "drug",
        "cell_name",
        "organ",
        "conc",
        if (has_full) "n_full",
        "n_cells"
      )
      df <- df[order(df[[power_col()]]), cols]
      df$conc <- paste0(df$conc, " µM")
      df
    })
    under_cols <- function(df) {
      coldefs <- list(conc = reactable::colDef(name = "Dose"))
      if ("n_full" %in% names(df)) {
        coldefs$n_full <- reactable::colDef(name = "QC-pass cells")
      }
      coldefs
    }
    tahoe_table_server(
      "under_table",
      data = under_display,
      columns = under_cols,
      pagination = FALSE,
      height = "34vh",
      empty_message = "No underpowered conditions at this threshold."
    )

    dose_display <- reactive({
      df <- incomplete()
      if (is.null(df) || nrow(df) == 0) {
        return(df)
      }
      df[
        order(df$n_doses),
        c("drug", "cell_name", "organ", "doses", "n_doses")
      ]
    })
    tahoe_table_server(
      "dose_table",
      data = dose_display,
      columns = list(
        doses = reactable::colDef(name = "Doses present (µM)"),
        n_doses = reactable::colDef(name = "# Doses")
      ),
      pagination = FALSE,
      height = "34vh",
      empty_message = "Every drug × cell-line combo has the full dose series."
    )

    output$control_plot <- plotly::renderPlotly({
      tahoe_plotly(.qc_control_bar(control_by_line()), tooltip = "text")
    })

    output$plate_note <- renderUI({
      g <- tahoe_cell_grid()
      validate(need(nrow(g) > 0, "No grid data."))
      n_lines <- dplyr::n_distinct(g$cell_name)
      n_plates <- dplyr::n_distinct(g$plate)
      per_line <- tapply(g$plate, g$cell_name, function(x) length(unique(x)))
      balanced <- all(per_line == n_plates)
      div(
        class = "small text-muted mb-2",
        if (balanced) {
          sprintf(
            paste(
              "All %d cell lines appear on all %d plates — no cell-line/plate",
              "confounding. Drugs, however, vary (below): pair each with",
              "same-plate DMSO controls."
            ),
            n_lines,
            n_plates
          )
        } else {
          sprintf(
            "Cell lines span %d–%d plates; check for batch imbalance.",
            min(per_line),
            max(per_line)
          )
        }
      )
    })

    output$plate_plot <- plotly::renderPlotly({
      tahoe_plotly(.qc_plate_bar(plates_per_drug()), tooltip = "text")
    })

    output$phase_plot <- plotly::renderPlotly({
      tahoe_plotly(.qc_phase_bar(phase_by_organ()), tooltip = "text")
    })
  })
}
