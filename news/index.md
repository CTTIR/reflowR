# Changelog

## reflowR 0.1.0

Initial release.

- [`reflow_init()`](https://r-heller.github.io/reflowR/reference/reflow_init.md)
  – main function wrapping
  [`workflowr::wflow_start()`](https://workflowr.github.io/workflowr/reference/wflow_start.html)
  with theming.
- Five color schemes: clinical (red), basic (steelblue), code (forest
  green), special (purple), other (grey).
- Three depth presets: minimal (3 steps), standard (8 steps), extended
  (12 steps).
- Custom SVG icons per scheme embedded in navbar.
- CSS/SCSS theme generation from color scheme definitions.
- [`reflow_preview()`](https://r-heller.github.io/reflowR/reference/reflow_preview.md)
  for interactive color preview.
- [`reflow_schemes()`](https://r-heller.github.io/reflowR/reference/reflow_schemes.md)
  and
  [`reflow_presets()`](https://r-heller.github.io/reflowR/reference/reflow_presets.md)
  convenience functions.
- Full compatibility with workflowr commands (wflow_build,
  wflow_publish, etc.).
- Two vignettes: Getting Started and Color Schemes Reference.
