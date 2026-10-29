# Always verify a compact artifact selection in targets

Creates an optional targets RDS target with an always cue. Every make
calls the selector; the stored value is only a compact reference. No
graph payloads are copied. A targets cache read alone does not verify
current files: consumers must call
[`reflow_artifact_selection_verify`](https://cttir.github.io/reflowR/dev/reference/reflow_artifact_selection_verify.md)
immediately before use. Plan expressions must not embed raw payloads.

## Usage

``` r
reflow_artifact_target(name, plan_command, registry)
```

## Arguments

- name:

  A plain target name.

- plan_command:

  One unevaluated language object constructing the plan.

- registry:

  Canonical selector registry path, initialized before execution.

## Value

A targets target object.

## Examples

``` r
if (requireNamespace("targets", quietly = TRUE)) {
  registry <- file.path(normalizePath(tempdir(), winslash = "/", mustWork = TRUE), "registry")
  # The plan expression is stored unevaluated and runs at every make
  target <- reflow_artifact_target("selection", quote(my_plan()), registry)
  target$command$expr
}
#> expression(reflowR::reflow_artifact_select(my_plan(), "/tmp/RtmpesQ9b9/registry"))
```
