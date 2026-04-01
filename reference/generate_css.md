# Generate Theme CSS from a Color Scheme

Creates a CSS file with custom properties and styled selectors based on
the provided color scheme.

## Usage

``` r
generate_css(scheme, output_path)
```

## Arguments

- scheme:

  A scheme list from
  [`reflow_scheme()`](https://rabanheller.github.io/reflowR/reference/reflow_scheme.md).

- output_path:

  Character. File path for the output CSS file.

## Value

Invisibly returns the output path.
