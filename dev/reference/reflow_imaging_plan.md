# Assemble a sequential imaging dependency graph

Stages must be in dependency order. Forward references and cycles fail.
File inputs must use
[`reflow_imaging_input()`](https://cttir.github.io/reflowR/dev/reference/reflow_imaging_ref.md)
so content changes invalidate a resume. Plain character arguments are
configuration, not tracked file inputs.

## Usage

``` r
reflow_imaging_plan(..., packages = character())
```

## Arguments

- ...:

  Stage specifications.

- packages:

  Additional installed packages to fingerprint, for example optional
  backends used by a stage. Declare these explicitly.

## Value

A versioned plan; no stage is executed.

## Examples

``` r
plan <- reflow_imaging_plan(
  reflow_imaging_stage("values", "base", "identity",
                       list(x = c(2, 4, 4, 5))),
  reflow_imaging_stage("mean", "base", "mean",
                       list(x = reflow_imaging_ref("values")))
)
vapply(plan$stages, `[[`, character(1), "id")
#> [1] "values" "mean"  
```
