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
        "
      ),
      fillable = FALSE
    ),
    app_nav_panels()
  )
)
