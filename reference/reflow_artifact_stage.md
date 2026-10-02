# Declare a deferred artifact writer

This additive API permits
[`reflow_artifact_ref()`](https://cttir.github.io/reflowR/reference/reflow_artifact_ref.md)
in ordinary arguments. Static declarations are validated immediately;
future artifact paths are resolved only after their producers have been
authenticated. Scientific methods remain in trusted installed exports.
No writer runs here.

## Usage

``` r
reflow_artifact_stage(
  id,
  package,
  fun,
  args = list(),
  outputs,
  inventory,
  allow_empty = character(),
  seed = NULL,
  packages = character(),
  runtime_files = character()
)
```

## Arguments

- id:

  Unique stage identifier.

- package, fun, args, outputs, inventory, allow_empty, seed, packages,
  runtime_files:

  See
  [`reflow_artifact_spec()`](https://cttir.github.io/reflowR/reference/reflow_artifact_spec.md).

## Value

A deferred writer declaration.
