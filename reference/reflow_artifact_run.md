# Execute or verify one declared artifact bundle

Fresh runs require an absent destination with an existing parent. Resume
revalidates a READY bundle without invoking its writer.
Failed/interrupted attempts require explicit caller reconciliation; no
subprocess absence is inferred from an R error or owner PID. Stale locks
require manual inspection. Orphan attempts with missing state always
require a new run after external reconciliation; supplying retry
arguments does not bypass this refusal. Receipts use atomic rename
visibility, not a power-loss durability guarantee. Writers are trusted:
observable input changes are detected, but transient restored changes
and escaped subprocesses are not prevented.

## Usage

``` r
reflow_artifact_run(spec, directory)

reflow_artifact_resume(
  spec,
  directory,
  reconciled_attempt = NULL,
  reconciliation = NULL
)
```

## Arguments

- spec:

  A
  [`reflow_artifact_spec()`](https://cttir.github.io/reflowR/reference/reflow_artifact_spec.md).

- directory:

  New run directory, or an existing directory for resume.

- reconciled_attempt:

  Exact latest failed/interrupted attempt identifier.

- reconciliation:

  Nonempty caller assertion explaining that all writer and subprocess
  activity has stopped. Required together with the identifier before a
  fresh retry. This is not automatic process verification.

## Value

An authenticated descriptor with stable bundle path and file manifest.
