# Genes tab — searchable lookup of the measured features (gene metadata).

register_page(
  id = "genes",
  title = "Genes",
  order = 25,
  ui = div(
    class = "p-2",
    h3("Gene explorer"),
    p(
      class = "text-muted",
      "Search the measured features and check whether specific genes were",
      "quantified before planning a targeted reanalysis."
    ),
    gene_explorer_ui("genes")
  ),
  server = function() gene_explorer_server("genes")
)
