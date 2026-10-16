# Preview a Color Scheme

Opens an HTML preview in the RStudio viewer (or browser) showing color
swatches, the scheme icon, and the navbar gradient.

## Usage

``` r
reflow_preview(scheme = "basic")
```

## Arguments

- scheme:

  Character. Scheme name (e.g., `"clinical"`).

## Value

Invisibly returns the scheme list.

## Examples

``` r
if (FALSE) { # \dontrun{
reflow_preview("clinical")
reflow_preview("basic")
} # }
```
