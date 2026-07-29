# Project Structure

```txt
.
├-- global.R
├-- ui.R
├-- server.R
├-- R/
├-- modules/
├-- userInterface/
├-- data/
├-- dev/
├-- docs/
└-- www/
```

## Notes

- This app uses Shiny's multi-file format: `shiny::runApp()` loads `global.R`,
  then `ui.R` (defines the UI) and `server.R` (defines the server function).
- `global.R` loads dependencies and sources components.
- `R/load_components.R` automatically sources modules and page UI files.
- `www/` stores static assets such as CSS, JavaScript, and images.
