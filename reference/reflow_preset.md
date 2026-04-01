# Get a Depth Preset Definition

Returns the full preset list for a given preset name.

## Usage

``` r
reflow_preset(name)
```

## Arguments

- name:

  Character. One of `"minimal"`, `"standard"`, `"extended"`.

## Value

A named list containing the preset name, label, file names, and
descriptions.

## See also

[`reflow_presets()`](https://rabanheller.github.io/reflowR/reference/reflow_presets.md)
for listing all available presets.

## Examples

``` r
p <- reflow_preset("standard")
p$files
#> [1] "01_read_data"     "02_tidy"          "03_preprocess"    "04_descriptive"  
#> [5] "05_inference"     "06_modelling"     "07_dim_reduction" "08_sub_reports"  
p$descriptions
#> $`01_read_data`
#> [1] "Import raw data from data/raw/"
#> 
#> $`02_tidy`
#> [1] "Clean, reshape, and validate data"
#> 
#> $`03_preprocess`
#> [1] "Handle missing data, outliers, transformations"
#> 
#> $`04_descriptive`
#> [1] "Table 1, distributions, correlations"
#> 
#> $`05_inference`
#> [1] "Hypothesis testing and group comparisons"
#> 
#> $`06_modelling`
#> [1] "Predictive modelling with tidymodels"
#> 
#> $`07_dim_reduction`
#> [1] "PCA, UMAP, t-SNE projections"
#> 
#> $`08_sub_reports`
#> [1] "Subgroup analyses and parameterized reports"
#> 
```
