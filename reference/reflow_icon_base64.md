# Get Base64-Encoded SVG Icon for a Scheme

Reads the scheme-specific SVG icon from the package's `inst/icons/`
directory and returns it as a base64-encoded data URI suitable for
embedding in HTML.

## Usage

``` r
reflow_icon_base64(scheme_name)
```

## Arguments

- scheme_name:

  Character. The scheme name (e.g., `"clinical"`).

## Value

A character string containing the base64 data URI (e.g.,
`"data:image/svg+xml;base64,..."`).

## Examples

``` r
if (FALSE) { # \dontrun{
uri <- reflow_icon_base64("clinical")
} # }
```
