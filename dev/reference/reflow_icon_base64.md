# Get Base64-Encoded SVG Icon for a Scheme

Reads the CTTIR v5 family icon from the package's `inst/icons/`
directory and returns it as a base64-encoded data URI suitable for
embedding in HTML. The icon is the v5 hex on a light surface: a tinted
face, the family accent as rim and a small family glyph, with a
transparent exterior.

## Usage

``` r
reflow_icon_base64(scheme_name)
```

## Arguments

- scheme_name:

  Character. A v5 family key (e.g., `"clinical"`) or one of the earlier
  scheme names, which resolve to their v5 family icon.

## Value

A character string containing the base64 data URI (e.g.,
`"data:image/svg+xml;base64,..."`).

## Examples

``` r
# \donttest{
uri <- reflow_icon_base64("clinical")
nchar(uri)
#> [1] 714
# }
```
