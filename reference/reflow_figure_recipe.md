# Declare a Finalized-Value Figure Recipe

Supports tile heatmaps and effect points only. All values, categorical
level orders and the heatmap midpoint are supplied by the caller. No
observations are filtered, aggregated or interpreted as independent
units.

## Usage

``` r
reflow_figure_recipe(
  type,
  mapping,
  row_key,
  levels,
  facets = list(),
  labels = list(),
  midpoint = NULL,
  reference = NULL,
  style = list(),
  status = "available",
  provenance = character()
)
```

## Arguments

- type:

  Either `tile_heatmap` or `effect_points`.

- mapping:

  Named character vector: heatmaps require `x`, `y`, `value`; effects
  require `x`, `y`, `colour`. Values are data column names.

- row_key:

  Nonempty character vector of complete unique-key columns.

- levels:

  Named list of explicit unique character levels. Required entries are
  x/y for heatmaps, y/colour for effects, plus facet_rows/facet_columns
  whenever those facets are requested. Nonempty data require exact
  coverage.

- facets:

  Named list with optional rows/columns column names. Facets use free y
  scales and free y space. They do not change numerical payloads.

- labels:

  Named list of optional title/x/y/fill/colour/caption text. Explicit
  NULL removes a label; empty text remains a distinct label.

- midpoint:

  Required finite scalar for available heatmaps; empty heatmaps may
  supply NULL. Must be NULL for effects.

- reference:

  Finite effect reference line; NULL for heatmaps.

- style:

  Named list of presentation overrides: base_size, family, low, mid,
  high, point_size, tile_linewidth, x_angle, strip_y_angle (right-side
  facet text, default -90). No expressions allowed.

- status:

  Explicit available or empty. Must agree with the supplied rows.

- provenance:

  Named character vector of caller declarations, not verified evidence.
  It is excluded from the scientific payload hash.

## Value

A versioned recipe; no data are processed or files written.
