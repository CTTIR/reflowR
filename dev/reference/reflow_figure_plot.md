# Build a Plot from a Finalized-Value Figure Recipe

Build a Plot from a Finalized-Value Figure Recipe

## Usage

``` r
reflow_figure_plot(data, recipe, annotation_data = NULL)
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
  [`reflow_figure_recipe()`](https://cttir.github.io/reflowR/dev/reference/reflow_figure_recipe.md).

- annotation_data:

  NULL for existing recipes. Wrapped numeric points require a separate
  frame with exactly one uniquely keyed row per declared facet. Selected
  key, facet and label columns must be plain nonmissing character
  vectors without attributes. Keys and facet values must each be unique;
  extra columns are ignored. Row order, literal newlines and supplied
  status/NA text are preserved; no statistics are computed.

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
status columns are not authenticated by these hashes. Wrapped numeric
points additionally return annotation_payload_sha256 for key-sorted
selected annotation values and declared key/facet/label columns.
Canonical payload hashes do not record input row order; drawing
preserves it and order equality requires separate diagnostics. Hashes
use R version-2 serialization, not a cross-language protocol.

## Details

Numeric points draw the supplied coordinates without aggregation,
filtering, fitting, jitter or automatic offsets. Text nudges affect
annotations only. Device-specific visible-label coverage, clipping and
minimum exported font sizes require separate render and visual
qualification. No files are written.

## Examples

``` r
scores <- data.frame(
  id = c("r1", "r2", "r3", "r4"),
  sample = c("s1", "s2", "s1", "s2"),
  marker = c("CD3", "CD3", "CD8", "CD8"),
  score = c(-1.2, 0.4, 0.8, 1.5)
)
recipe <- reflow_figure_recipe(
  "tile_heatmap",
  mapping = c(x = "sample", y = "marker", value = "score"),
  row_key = "id",
  levels = list(x = c("s1", "s2"), y = c("CD3", "CD8")),
  midpoint = 0,
  labels = list(title = "Finalized scores", fill = "z-score")
)
figure <- reflow_figure_plot(scores, recipe)
figure$rows
#> [1] 4
figure$payload_sha256
#> [1] "6c4b9c949848d0b20ada7f1b9f5326fc27a2cc4edcb9516cd938a2ba37ba72ab"
figure$plot
```
