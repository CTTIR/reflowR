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
