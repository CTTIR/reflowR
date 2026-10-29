# Generate Analysis Rmd Files

Creates individual Rmd files for the analysis pipeline based on the
selected depth preset.

## Usage

``` r
generate_rmd(project_dir, file_name, author, description, scheme, preset)
```

## Arguments

- project_dir:

  Character. Path to the project root.

- file_name:

  Character. Base file name (e.g., `"01_read_data"`).

- author:

  Character. Author name.

- description:

  Character. Step description.

- scheme:

  A scheme list from
  [`reflow_scheme()`](https://cttir.github.io/reflowR/reference/reflow_scheme.md).

- preset:

  A preset list from
  [`reflow_preset()`](https://cttir.github.io/reflowR/reference/reflow_preset.md).

## Value

Invisibly returns the output file path.
