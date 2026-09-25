# Changelog

## reflowR (development version)

- Record common numerical thread environment settings in imaging
  definitions and runtime fingerprints; reject resume when those
  settings change.

- Add a sequential imaging plan/run/resume API for explicit exported
  package calls, upstream object references and tracked file or
  directory inputs.

- Record content, configuration, installed code, runtime and result
  hashes in durable receipts; reject stale or corrupt caches and
  concurrent run attempts. Retries preserve prior attempt artifacts.
  These APIs do not initialize Git, publish, download data or install
  packages.

- This first orchestration slice returns R objects; resource scheduling,
  external engine qualification, dashboards and report recipes remain
  separate work.

## reflowR 0.1.0

Initial release.

- [`reflow_init()`](https://cttir.github.io/reflowR/reference/reflow_init.md)
  – main function wrapping
  [`workflowr::wflow_start()`](https://workflowr.github.io/workflowr/reference/wflow_start.html)
  with theming.
- Five color schemes: clinical (red), basic (steelblue), code (forest
  green), special (purple), other (grey).
- Three depth presets: minimal (3 steps), standard (8 steps), extended
  (12 steps).
- Custom SVG icons per scheme embedded in navbar.
- CSS/SCSS theme generation from color scheme definitions.
- [`reflow_preview()`](https://cttir.github.io/reflowR/reference/reflow_preview.md)
  for interactive color preview.
- [`reflow_schemes()`](https://cttir.github.io/reflowR/reference/reflow_schemes.md)
  and
  [`reflow_presets()`](https://cttir.github.io/reflowR/reference/reflow_presets.md)
  convenience functions.
- Full compatibility with workflowr commands (wflow_build,
  wflow_publish, etc.).
- Two vignettes: Getting Started and Color Schemes Reference.
