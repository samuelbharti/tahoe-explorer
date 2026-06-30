# Theming

This template themes the app from a single [`_brand.yml`](../_brand.yml) file
using the [brand.yml](https://posit-dev.github.io/brand-yml/) standard, applied
through [bslib](https://rstudio.github.io/bslib/).

## How it works

- `_brand.yml` defines the brand: color palette, semantic colors (primary,
  secondary, foreground, background), typography (fonts, sizes, weights), and an
  optional logo.
- [ui.R](../ui.R) calls `bslib::bs_theme(brand = TRUE)`, which discovers
  `_brand.yml` at the app root and applies it to the whole UI.
- `brand = TRUE` requires the file to exist (a clear contract). To make it
  optional, use `bslib::bs_theme()` instead — it applies `_brand.yml` if found
  and is a no-op otherwise.

## Customizing

Edit `_brand.yml`. For example, to change the primary color and base font:

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

Restart the app to see changes. The full set of fields is documented in the
[brand.yml specification](https://posit-dev.github.io/brand-yml/articles/brand-yml.html).

## Theming plots and tables

bslib themes the HTML/CSS UI, but R plots are drawn separately. Install
[`thematic`](https://rstudio.github.io/thematic/) to make base R, ggplot2, and
lattice graphics inherit the app's colors automatically:

```r
install.packages("thematic")
```

[global.R](../global.R) already calls `thematic::thematic_shiny(font = "auto")`
when the package is installed, so no further wiring is required. To also render
custom or Google fonts (such as Inter) in plots, install
[`showtext`](https://github.com/yixuan/showtext) — without it, thematic applies
the theme colors but falls back to the default graphics-device font.

## Notes

- `_brand.yml` is app content, not template scaffolding — `dev/use_template.R`
  keeps it and only updates `meta.name` to your project name.
- Keep `_brand.yml` in version control so the look of the app is reproducible.
