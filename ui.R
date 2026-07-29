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
      # The Sass below layers an "organic" pass over the Alpine Lake palette:
      # a soft misted-water background, rounded corners, and gentle teal-tinted
      # depth, so the app reads as natural rather than flat/boxy.
      theme = bslib::bs_add_rules(
        bslib::bs_theme(brand = TRUE),
        "
        /* Organic palette anchors (mirror _brand.yml). */
        :root {
          --tahoe-teal: #0B7285;
          --tahoe-green: #2F9E44;
          --tahoe-snow: #F8FAFB;
        }
        /* Misted-water backdrop: two faint colour pools over snow, fixed so it
           stays put while content scrolls. */
        body {
          background:
            radial-gradient(1100px 600px at 100% -8%,
              rgba(47, 158, 68, 0.06), transparent 60%),
            radial-gradient(1000px 520px at -8% 2%,
              rgba(11, 114, 133, 0.07), transparent 58%),
            var(--tahoe-snow);
          background-attachment: fixed;
        }
        /* Cards: softer corners, a hairline teal edge, and layered teal-tinted
           shadow that deepens gently on hover (organic lift, no jump). */
        .card {
          border: 1px solid rgba(11, 114, 133, 0.07);
          border-radius: 0.9rem;
          box-shadow: 0 1px 2px rgba(33, 37, 41, 0.04),
                      0 8px 22px rgba(11, 114, 133, 0.06);
          transition: box-shadow 0.25s ease;
        }
        .card:hover {
          box-shadow: 0 2px 4px rgba(33, 37, 41, 0.05),
                      0 14px 32px rgba(11, 114, 133, 0.10);
        }
        .card-header {
          background-color: transparent;
          border-bottom: 1px solid rgba(11, 114, 133, 0.10);
          font-weight: 600;
        }
        .bslib-value-box {
          border-radius: 0.9rem;
          box-shadow: 0 8px 22px rgba(11, 114, 133, 0.08);
        }
        /* Rounded, tactile controls. */
        .btn { border-radius: 0.6rem; }
        .form-control, .form-select, .selectize-input {
          border-radius: 0.6rem;
        }
        /* Frosted navbar so the backdrop shows through faintly. */
        .navbar {
          background-color: rgba(248, 250, 251, 0.85) !important;
          backdrop-filter: saturate(1.15) blur(8px);
          box-shadow: 0 1px 0 rgba(11, 114, 133, 0.08);
        }
        /* The navbar Demo button: a rounded, on-brand call to action. */
        #demo_tour {
          border-radius: 2rem;
          font-weight: 600;
        }
        /* Consistent horizontal gutter on every page's content (not the navbar
           menu). Applied to the tab-pane so fillable pages (Chat) match too. */
        .bslib-page-navbar > .container-fluid > .tab-content > .tab-pane {
          padding-left: 4vw;
          padding-right: 4vw;
        }
        "
      ),
      # Load the guided-tour (cicerone) JS/CSS dependency once for the whole app.
      header = cicerone::use_cicerone(),
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
    app_nav_panels(),
    # Right-aligned navbar actions (nav_spacer pushes everything after it to the
    # right): a guided-demo launcher and a data-provenance chip. The Demo button
    # switches to the relevant tab and starts the cicerone tour (server.R /
    # R/tour.R); the chip shows whether the numbers are real data or fixtures.
    list(
      bslib::nav_spacer(),
      bslib::nav_item(
        actionButton(
          "demo_tour",
          "Demo",
          icon = icon("circle-play"),
          class = "btn-sm btn-primary my-1"
        )
      ),
      bslib::nav_item(tahoe_provenance_badge())
    )
  )
)
