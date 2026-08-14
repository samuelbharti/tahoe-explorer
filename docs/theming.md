# Theming

This app takes its theme from one [`_brand.yml`](../_brand.yml) file. The file
obeys the [brand.yml](https://posit-dev.github.io/brand-yml/) standard, and
[bslib](https://rstudio.github.io/bslib/) applies it.

## How it works

- `_brand.yml` defines the brand. It holds the color palette, the semantic
  colors, the typography, and an optional logo.
- [ui.R](../ui.R) calls `bslib::bs_theme(brand = TRUE)`. This call finds
  `_brand.yml` at the app root and applies it to the whole UI.
- `brand = TRUE` needs the file to be present. This behavior is a clear
  contract.

Note: to make the file optional, use `bslib::bs_theme()` instead. That function
applies `_brand.yml` if it is present, and does nothing if it is absent.

## How to customize the theme

1. Edit `_brand.yml`.
2. Restart the app.

For example, this configuration changes the primary color and the base font:

```yaml
color:
  palette:
    blue: "#1d4ed8"
  primary: blue

typography:
  fonts:
    - family: Roboto
      source: google
      weight: [400, 600]
  base: Roboto
```

The [brand.yml specification](https://posit-dev.github.io/brand-yml/articles/brand-yml.html)
lists every field.

## Themes for plots and tables

bslib themes the HTML and the CSS of the UI. R draws the plots separately.

To give base R, ggplot2, and lattice graphics the colors of the app, install
[`thematic`](https://rstudio.github.io/thematic/):

```r
install.packages("thematic")
```

[global.R](../global.R) already calls `thematic::thematic_shiny(font = "auto")`
when the package is present. No more configuration is necessary.

To show custom fonts or Google fonts in the plots, also install
[`showtext`](https://github.com/yixuan/showtext). Without `showtext`, thematic
applies the theme colors but uses the default font of the graphics device.

`R/theme.R` holds the chart palettes and the helper functions `tahoe_theme()`,
`tahoe_plotly()`, and `tahoe_reactable()`.

## Notes

- `_brand.yml` is app content, not build scaffolding. `meta.name` carries the
  project name.
- Keep `_brand.yml` in version control. The appearance of the app is then
  reproducible.
