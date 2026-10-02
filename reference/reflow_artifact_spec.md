# Declare one immutable file-producing bundle

Calls one trusted installed export, substituting only declared top-level
output arguments. The writer's return value is discarded. This is not a
sandbox, scheduler, native-process supervisor or complete artifact DAG.
Plain strings are configuration; files must use
[`reflow_imaging_input()`](https://cttir.github.io/reflowR/reference/reflow_imaging_ref.md).
Paths reject symbolic links, special files, reserved Windows components
and trailing dots/spaces. Unicode spelling is preserved, not normalized.
Parents of output paths are created; declared output directories are
writer-created. Installed package resources (including templates),
tracked inputs and declared external runtime files are fingerprinted.
Undeclared ambient state is not.

## Usage

``` r
reflow_artifact_spec(
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

- package, fun:

  Installed package and exported writer function.

- args:

  Uniquely named ordinary arguments, excluding output arguments.

- outputs:

  Named list keyed by output argument, each an exact list with `path`
  (relative to the bundle) and `type` (`file` or `directory`).

- inventory:

  Nonempty named character vector of logical artifact names mapped to
  exact relative file paths. Unexpected directories also fail.

- allow_empty:

  Logical artifact names permitted to contain zero bytes.

- seed:

  Explicit nonnegative integer seed, or NULL for a nonrandom writer.

- packages:

  Additional installed packages to fingerprint.

- runtime_files:

  Existing external runtime files to fingerprint. The caller must ensure
  the writer actually dispatches these binaries/resources.

## Value

A versioned declaration; no writer is invoked.
