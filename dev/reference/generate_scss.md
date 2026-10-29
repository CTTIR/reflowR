# Generate Theme SCSS from a Colour Scheme

Creates a Quarto-compatible SCSS file on the CTTIR v5 surface. Bootstrap
defaults take the light tokens (links use the link token, as in
themakR); the rules section adds the same `--cttir-*` custom properties
as the site CSS, a dark mode and the accent identity rules.

## Usage

``` r
generate_scss(scheme, output_path)
```

## Arguments

- scheme:

  A scheme list from
  [`reflow_scheme()`](https://cttir.github.io/reflowR/dev/reference/reflow_scheme.md).

- output_path:

  Character. File path for the output SCSS file.

## Value

Invisibly returns the output path.
