# Preview a Colour Scheme

Opens an HTML preview in the RStudio viewer (or browser) showing the
family icon on a flat navbar with its accent rule, the light and dark
surfaces, the identity colours and the Okabe-Ito palette.

## Usage

``` r
reflow_preview(scheme = "brand")
```

## Arguments

- scheme:

  Character. A v5 family key (e.g., `"clinical"`) or an earlier scheme
  name, see
  [`reflow_scheme()`](https://cttir.github.io/reflowR/reference/reflow_scheme.md).

## Value

Invisibly returns the scheme list.

## Examples

``` r
if (interactive()) {
  reflow_preview("clinical")
  reflow_preview("brand")
}
```
