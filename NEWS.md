# reflowR (development version)

* Add a separate declared artifact bundle API for trusted installed writers,
  complete file/resource fingerprints, immutable accepted receipts and verified
  reuse. Failed attempts require explicit process reconciliation before retry;
  this is not a scheduler, sandbox or complete artifact DAG.

* Allow an explicit right-side facet-strip text angle in finalized figure
  recipes; the default preserves the existing -90-degree presentation.

* Build finalized tile heatmaps and effect-point plots from explicit keyed
  recipes, supplied categorical orders and midpoints, without aggregation or
  filtering. Keep scientific payload hashes separate from styling/provenance.

* Record common numerical thread environment settings in imaging definitions
  and runtime fingerprints; reject resume when those settings change.

* Add a sequential imaging plan/run/resume API for explicit exported package calls,
  upstream object references and tracked file or directory inputs.
* Record content, configuration, installed code, runtime and result hashes in
  durable receipts; reject stale or corrupt caches and concurrent run attempts.
  Retries preserve prior attempt artifacts. These APIs do not initialize Git,
  publish, download data or install packages.
* This first orchestration slice returns R objects; resource scheduling, external
  engine qualification, dashboards and report recipes remain separate work.

# reflowR 0.1.0

Initial release.

* `reflow_init()` -- main function wrapping `workflowr::wflow_start()` with theming.
* Five color schemes: clinical (red), basic (steelblue), code (forest green), special (purple), other (grey).
* Three depth presets: minimal (3 steps), standard (8 steps), extended (12 steps).
* Custom SVG icons per scheme embedded in navbar.
* CSS/SCSS theme generation from color scheme definitions.
* `reflow_preview()` for interactive color preview.
* `reflow_schemes()` and `reflow_presets()` convenience functions.
* Full compatibility with workflowr commands (wflow_build, wflow_publish, etc.).
* Two vignettes: Getting Started and Color Schemes Reference.
