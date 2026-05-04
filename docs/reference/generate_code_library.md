# Generate the Code Library Files

Creates `code/formulary.R`, `code/data_io.R`, `code/table_helpers.R`,
and `code/plot_helpers.R` in the project directory.

## Usage

``` r
generate_code_library(project_dir, scheme, preset)
```

## Arguments

- project_dir:

  Character. Path to the project root.

- scheme:

  A scheme list from
  [`reflow_scheme()`](https://r-heller.github.io/reflowR/reference/reflow_scheme.md).

- preset:

  A preset list from
  [`reflow_preset()`](https://r-heller.github.io/reflowR/reference/reflow_preset.md).

## Value

Invisibly returns a character vector of created file paths.
