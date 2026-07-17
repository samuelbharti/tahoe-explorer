# App UI. Tabs self-register via register_page() (see R/app_pages.R), so this
# file rarely changes when features are added.
do.call(
  bslib::page_navbar,
  c(
    list(
      title = "Tahoe Explorer",
      id = "main_nav",
      # Apply branding from _brand.yml (colors, fonts). brand = TRUE requires
      # the file to exist; switch to bslib::bs_theme() to make it optional.
      # A few Sass rules add soft card shadows and a cleaner header/navbar.
      theme = bslib::bs_add_rules(
        bslib::bs_theme(brand = TRUE),
        "
        .card {
          border: none;
          box-shadow: 0 1px 3px rgba(33, 37, 41, 0.08),
                      0 1px 2px rgba(33, 37, 41, 0.05);
        }
        .card-header {
          background-color: transparent;
          border-bottom: 1px solid #e9ecef;
          font-weight: 600;
        }
        .navbar { box-shadow: 0 1px 0 rgba(33, 37, 41, 0.06); }
        .bslib-value-box { border-radius: 0.5rem; }
        /* Consistent horizontal gutter on every page's content (not the navbar
           menu). Applied to the tab-pane so fillable pages (Chat) match too. */
        .bslib-page-navbar > .container-fluid > .tab-content > .tab-pane {
          padding-left: 4vw;
          padding-right: 4vw;
        }
        "
      ),
      # Only pages that opt in (via register_page(fillable = TRUE)) become
      # fillable -- currently the Chat page, so its chat fills the viewport.
      fillable = app_fillable_pages(),
      # App-wide attribution footer (4vw gutter matches the page content).
      footer = tags$footer(
        class = "text-muted small border-top mt-3",
        style = "padding: 0.75rem 4vw;",
        div(
          class = paste(
            "d-flex flex-wrap justify-content-between align-items-center gap-2"
          ),
          span(HTML(
            "<strong>Tahoe Explorer</strong> &middot; built by Samuel Bharti",
            "&middot; MIT &copy; 2024&ndash;2026"
          )),
          span(
            "Data: ",
            tags$a(
              href = "https://huggingface.co/datasets/tahoebio/Tahoe-100M",
              target = "_blank",
              rel = "noopener",
              "Tahoe-100M"
            ),
            " · ",
            tags$a(
              href = "https://github.com/samuelbharti/tahoe-explorer",
              target = "_blank",
              rel = "noopener",
              "Source"
            )
          )
        )
      )
    ),
    app_nav_panels()
  )
)
