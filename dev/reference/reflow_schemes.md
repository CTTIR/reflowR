# List Available Color Schemes

Prints the CTTIR v5 topic families with their labels and accents,
followed by the earlier scheme names that remain accepted as aliases,
and returns the family keys invisibly.

## Usage

``` r
reflow_schemes()
```

## Value

Invisibly returns a character vector of the nine v5 family keys.

## Examples

``` r
reflow_schemes()
#> 
#> ── Available reflowR Schemes (CTTIR v5 families) ──
#> 
#> "spectral" -- Spectral imaging (#0072B2)
#> "spatial" -- Spatial biology (#009E73)
#> "omics" -- Molecular data (#CC79A7)
#> "clinical" -- Clinical science (#D55E00)
#> "methods" -- Learning & inference (#56B4E9)
#> "simulation" -- Systems & simulation (#E69F00)
#> "evidence" -- Evidence & discovery (#F0E442)
#> "platform" -- Research infrastructure (#000000)
#> "brand" -- Science, connected (#0072B2)
#> Earlier names remain aliases: "basic -> brand", "code -> methods", "special ->
#> evidence", and "other -> platform".
```
