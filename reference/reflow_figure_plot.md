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
  columns are ignored. Repeated display positions are retained as
  distinct keyed rows.

- recipe:

  A recipe from
  [`reflow_figure_recipe()`](https://cttir.github.io/reflowR/reference/reflow_figure_recipe.md).

## Value

Versioned list with ggplot or NULL for explicit empty input, status, row
count, selected columns, payload SHA-256 and recipe SHA-256. The payload
hash binds key-sorted selected values/mapping and supplied
midpoint/reference; styling, order declarations and unverified
provenance affect only recipe hash. Hashes use R version-2 serialization
and are not a cross-language protocol.
