# Declare a serial artifact dependency plan

Declaration order is immaterial: a stable identifier order breaks ties
among available stages. This dependency core is not a resource scheduler
or a targets adapter. Existing files use
[`reflow_imaging_input()`](https://cttir.github.io/reflowR/reference/reflow_imaging_ref.md);
no external execution is inferred from an input path. Plain strings are
configuration.

## Usage

``` r
reflow_artifact_plan(...)
```

## Arguments

- ...:

  Deferred
  [`reflow_artifact_stage()`](https://cttir.github.io/reflowR/reference/reflow_artifact_stage.md)
  declarations.

## Value

A canonical acyclic plan, without executing writers.

## Examples

``` r
plan <- reflow_artifact_plan(
  reflow_artifact_stage(
    "report", "base", "writeLines",
    args = list(text = reflow_artifact_ref("data", "labels", read = TRUE)),
    outputs = list(con = list(path = "report.txt", type = "file")),
    inventory = c(report = "report.txt")
  ),
  reflow_artifact_stage(
    "data", "base", "saveRDS",
    args = list(object = c("alpha", "beta")),
    outputs = list(file = list(path = "labels.rds", type = "file")),
    inventory = c(labels = "labels.rds")
  )
)
# Producers come before their consumers, whatever the declaration order
names(plan$stages)
#> [1] "data"   "report"
plan$dependencies
#> $data
#> character(0)
#> 
#> $report
#> [1] "data"
#> 
```
