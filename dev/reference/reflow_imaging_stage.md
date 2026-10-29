# Declare an imaging workflow stage

Stages call installed exported functions; they do not evaluate script
text. Scientific methods and validation remain in the called packages.
Calls must return serializable values and must not mutate their inputs
or write external outputs. This executor is not a sandbox: use trusted
package functions only.

## Usage

``` r
reflow_imaging_stage(id, package, fun, args = list(), seed = NULL)
```

## Arguments

- id:

  Stage identifier containing letters, digits, underscores; unique
  within a plan even when letter case is ignored.

- package:

  Installed package name.

- fun:

  Exported function name.

- args:

  Named list of arguments, including explicit references if needed.

- seed:

  Optional integer seed for stochastic calls. Calls consuming the
  ambient random stream without an explicit stage seed fail.

## Value

A stage specification.

## Examples

``` r
stage <- reflow_imaging_stage("summary", "base", "summary",
                              args = list(object = c(2, 4, 4, 5)))
stage$id
#> [1] "summary"
# Stochastic calls need an explicit seed
noisy <- reflow_imaging_stage("noise", "stats", "rnorm", list(n = 3), seed = 1L)
noisy$seed
#> [1] 1
```
