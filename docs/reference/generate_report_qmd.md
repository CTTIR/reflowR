# Generate Quarto Report Template

Creates a `report/report.qmd` file with dual HTML + PDF output using the
project's SCSS theme.

## Usage

``` r
generate_report_qmd(project_dir, name, author, scheme, preset)
```

## Arguments

- project_dir:

  Character. Path to the project root.

- name:

  Character. Project name.

- author:

  Character. Author name.

- scheme:

  A scheme list from
  [`reflow_scheme()`](https://r-heller.github.io/reflowR/reference/reflow_scheme.md).

- preset:

  A preset list from
  [`reflow_preset()`](https://r-heller.github.io/reflowR/reference/reflow_preset.md).

## Value

Invisibly returns the output file path.
