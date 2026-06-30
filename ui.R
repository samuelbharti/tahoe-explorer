navbarPage(
  title = "My App",
  # Apply branding from _brand.yml (colors, fonts). brand = TRUE requires the
  # file to exist; switch to bslib::bs_theme() to make it optional.
  theme = bslib::bs_theme(brand = TRUE),
  tabPanel("Home", home_page),
  tabPanel("Other", other_page)
)
