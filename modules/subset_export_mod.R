# Reusable export module.
#
# Downloads the current (filtered) data frame as CSV and/or parquet, and can
# optionally render a copy-paste "analysis recipe" supplied by the caller.
# Every explorer embeds this so the "pull a subset" behaviour is consistent.

subset_export_ui <- function(id, show_recipe = FALSE) {
  ns <- NS(id)
  tagList(
    div(
      class = "d-flex gap-2 flex-wrap align-items-center",
      downloadButton(
        ns("csv"),
        "Download CSV",
        class = "btn-sm btn-primary"
      ),
      downloadButton(
        ns("parquet"),
        "Download Parquet",
        class = "btn-sm btn-outline-primary"
      ),
      tags$span(
        class = "text-muted small",
        textOutput(ns("n_rows"), inline = TRUE)
      )
    ),
    if (show_recipe) {
      tagList(
        tags$hr(),
        tags$strong("Analysis recipe"),
        tags$p(
          class = "text-muted small",
          "Copy-paste code to reproduce this selection on the full dataset,",
          " or download it as a notebook to start from."
        ),
        verbatimTextOutput(ns("recipe")),
        div(
          class = "d-flex gap-2 flex-wrap align-items-end",
          div(
            style = "min-width: 210px;",
            selectInput(
              ns("recipe_format"),
              "Download as notebook",
              choices = c(
                "R Markdown (.Rmd)" = "rmd",
                "Quarto (.qmd)" = "qmd",
                "Jupyter (.ipynb)" = "ipynb"
              )
            )
          ),
          downloadButton(
            ns("recipe_dl"),
            "Download",
            class = "btn-sm btn-outline-primary mb-3"
          )
        )
      )
    }
  )
}

# Resolve a value that may be a plain object or a (reactive) function.
.export_resolve <- function(x, default = NULL) {
  if (is.function(x)) {
    return(tryCatch(x(), error = function(e) default))
  }
  if (is.null(x)) default else x
}

subset_export_server <- function(
  id,
  data_reactive,
  file_stem = "tahoe_subset",
  recipe = NULL,
  recipe_parts = NULL
) {
  moduleServer(id, function(input, output, session) {
    current_data <- reactive({
      df <- .export_resolve(data_reactive, default = NULL)
      if (is.null(df)) data.frame() else df
    })

    stem <- reactive({
      value <- .export_resolve(file_stem, default = "tahoe_subset")
      if (is.null(value) || !nzchar(value)) "tahoe_subset" else value
    })

    output$n_rows <- renderText({
      sprintf("%s rows selected", format(nrow(current_data()), big.mark = ","))
    })

    output$csv <- downloadHandler(
      filename = function() paste0(stem(), ".csv"),
      content = function(file) {
        utils::write.csv(current_data(), file, row.names = FALSE)
      }
    )

    output$parquet <- downloadHandler(
      filename = function() paste0(stem(), ".parquet"),
      content = function(file) {
        df <- current_data()
        if (nrow(df) == 0 && ncol(df) == 0) {
          df <- data.frame(note = character())
        }
        con <- tahoe_con()
        # Unique view name: the duckdb connection is shared process-wide, so a
        # fixed name could clash if downloads ever run re-entrantly (async).
        view <- paste0("export_tmp_", as.integer(stats::runif(1, 0, 2e9)))
        duckdb::duckdb_register(con, view, df)
        on.exit(duckdb::duckdb_unregister(con, view), add = TRUE)
        DBI::dbExecute(
          con,
          sprintf(
            "COPY %s TO '%s' (FORMAT PARQUET)",
            view,
            gsub("'", "''", file, fixed = TRUE)
          )
        )
      }
    )

    # The recipe text: prefer the structured parts (which also power the
    # notebook download) and fall back to a plain recipe string.
    if (!is.null(recipe_parts)) {
      output$recipe <- renderText({
        parts <- .export_resolve(recipe_parts, default = NULL)
        if (is.null(parts)) "" else parts$recipe %||% ""
      })
    } else if (!is.null(recipe)) {
      output$recipe <- renderText({
        .export_resolve(recipe, default = "")
      })
    }

    if (!is.null(recipe_parts)) {
      recipe_ext <- c(rmd = ".Rmd", qmd = ".qmd", ipynb = ".ipynb")
      output$recipe_dl <- downloadHandler(
        filename = function() {
          fmt <- input$recipe_format %||% "rmd"
          paste0(stem(), "_recipe", recipe_ext[[fmt]])
        },
        content = function(file) {
          parts <- .export_resolve(recipe_parts, default = NULL)
          fmt <- input$recipe_format %||% "rmd"
          text <- if (is.null(parts)) {
            "No selection to export."
          } else {
            tahoe_subset_document(parts, format = fmt)
          }
          writeLines(text, file, useBytes = TRUE)
        }
      )
    }

    invisible(current_data)
  })
}
