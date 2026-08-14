# Project Structure

```txt
.
├-- _brand.yml          # Brand colors and fonts
├-- global.R            # Libraries and global objects
├-- ui.R                # App UI
├-- server.R            # App server
├-- manifest.json       # Posit Connect Cloud deployment metadata
├-- R/                  # Utilities, data layer, page registry
├-- modules/            # Reusable Shiny modules, one for each tab
├-- userInterface/      # Page definitions that register their own tabs
├-- inst/agent/prompts/ # Prompt text for the optional assistant
├-- data/               # Curated tables and synthetic fixtures
├-- dev/                # Download, fixture, and manifest scripts
├-- tests/testthat/     # Unit, module, and browser tests
├-- docs/               # Project documentation
└-- www/                # Static assets
```

## Notes

- The app uses the multi-file format of Shiny. `shiny::runApp()` loads
  `global.R` first. It then loads `ui.R` and `server.R`.
- `global.R` loads the dependencies and sources the components.
- `R/load_components.R` sources the files in `R/`, `modules/`, and
  `userInterface/`.
- Each file in `userInterface/` registers its own tab through the page registry
  in `R/app_pages.R`. The navbar order comes from these registrations.
- `www/` holds static assets such as CSS, JavaScript, and images.
- `data/` holds the six small curated tables and the synthetic fixtures under
  `data/fixtures/`. The 2.29 GB cell-level `obs` table is not in the repository.
