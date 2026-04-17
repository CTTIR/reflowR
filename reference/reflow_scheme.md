# Get a Color Scheme Definition

Returns the full color scheme list for a given scheme name.

## Usage

``` r
reflow_scheme(name)
```

## Arguments

- name:

  Character. One of `"clinical"`, `"basic"`, `"code"`, `"special"`,
  `"other"`.

## Value

A named list containing color values, palette, label, and icon info.

## See also

[`reflow_schemes()`](https://r-heller.github.io/reflowR/reference/reflow_schemes.md)
for listing all available schemes.

## Examples

``` r
s <- reflow_scheme("clinical")
s$primary
#> [1] "#C8102E"
s$palette
#> [1] "#C8102E" "#E63946" "#A50021" "#F4A261" "#E76F51" "#2A9D8F" "#264653"
#> [8] "#6C757D"
```
