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
      theme = bslib::bs_theme(brand = TRUE),
      fillable = FALSE
    ),
    app_nav_panels()
  )
)
