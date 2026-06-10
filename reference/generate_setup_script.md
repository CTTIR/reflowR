# Generate the 00_setup.R Script

Creates the project setup script with package loading, paths, color
definitions, ggplot2 theme, and table styling helpers.

## Usage

``` r
generate_setup_script(project_dir, scheme)
```

## Arguments

- project_dir:

  Character. Path to the project root.

- scheme:

  A scheme list from
  [`reflow_scheme()`](https://cttir.github.io/reflowR/reference/reflow_scheme.md).

## Value

Invisibly returns the output file path.
