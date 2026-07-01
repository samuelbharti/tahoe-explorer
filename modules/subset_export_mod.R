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
          "Copy-paste code to reproduce this selection on the full dataset."
        ),
        verbatimTextOutput(ns("recipe"))
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
  recipe = NULL
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
        duckdb::duckdb_register(con, "export_tmp", df)
        on.exit(duckdb::duckdb_unregister(con, "export_tmp"), add = TRUE)
        DBI::dbExecute(
          con,
          sprintf(
            "COPY export_tmp TO '%s' (FORMAT PARQUET)",
            gsub("'", "''", file, fixed = TRUE)
          )
        )
      }
    )

    if (!is.null(recipe)) {
      output$recipe <- renderText({
        .export_resolve(recipe, default = "")
      })
    }

    invisible(current_data)
  })
}
