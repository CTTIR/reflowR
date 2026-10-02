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
