# Build a Plot from a Finalized-Value Figure Recipe

Build a Plot from a Finalized-Value Figure Recipe

## Usage

``` r
reflow_figure_plot(data, recipe)
```

## Arguments

- data:

  Data frame with plain finite numeric plotted values and nonblank
  character keys/categories. Complete row keys must be unique. Extra
  columns are ignored without modifying caller input. Repeated display
  positions remain distinct keyed rows. Numeric points require nonempty
  data and preserve input row order, with complete colour/facet levels
  and fixed numeric scales.

- recipe:

  A recipe from
  [`reflow_figure_recipe()`](https://cttir.github.io/reflowR/reference/reflow_figure_recipe.md).

## Value

Versioned list with plot (ggplot, or NULL for explicitly empty legacy
input), status, rows, selected_columns, payload_sha256 and
recipe_sha256. Numeric points additionally return annotation_records
(zero when absent) and visible_annotations = `NA_integer_`: retained
records do not establish device-visible labels. Clipping and
check_overlap can suppress visibility. The payload hash binds key-sorted
selected values and mapping, row keys, and supplied midpoint/reference
as applicable. Styling, level orders, numeric axis declarations, colour
maps and unverified provenance bind the recipe hash. Extra unselected
status columns are not authenticated by these hashes. Hashes use R
version-2 serialization, not a cross-language protocol.

## Details

Numeric points draw the supplied coordinates without aggregation,
filtering, fitting, jitter or automatic offsets. Text nudges affect
annotations only. Device-specific visible-label coverage, clipping and
minimum exported font sizes require separate render and visual
qualification. No files are written.
