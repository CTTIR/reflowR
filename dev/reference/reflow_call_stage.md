# Declare a stage that calls one installed export

Builds the
[`reflow_artifact_stage()`](https://cttir.github.io/reflowR/dev/reference/reflow_artifact_stage.md)
for
[`reflow_call_writer()`](https://cttir.github.io/reflowR/dev/reference/reflow_call_writer.md):
the stage saves the value of `fun` as the artifact `value` (`value.rds`)
and a tab-separated copy as `table` (`value.tsv`, empty for values that
are not plain data frames). The package of `fun` and any `packages` are
fingerprinted. Refer to the value downstream with
`reflow_artifact_ref(id, "value", read = TRUE)`.

## Usage

``` r
reflow_call_stage(id, fun, args = list(), seed = NULL, packages = character())
```

## Arguments

- id:

  Unique stage identifier.

- fun:

  One string `"package::function"` naming an exported function.

- args:

  Uniquely named list of arguments; may contain
  [`reflow_artifact_ref()`](https://cttir.github.io/reflowR/dev/reference/reflow_artifact_ref.md)
  and
  [`reflow_imaging_input()`](https://cttir.github.io/reflowR/dev/reference/reflow_imaging_ref.md)
  values at any depth.

- seed:

  Explicit nonnegative integer seed, or NULL for a nonrandom call.

- packages:

  Additional installed packages to fingerprint, such as optional
  packages that `fun` uses.

## Value

A deferred writer declaration.

## Examples

``` r
stage <- reflow_call_stage("top", "utils::head", list(x = 1:10, n = 3))
stage$declaration$inventory
#>       value       table 
#> "value.rds" "value.tsv" 
```
