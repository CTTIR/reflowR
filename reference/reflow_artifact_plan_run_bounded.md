# Run or resume a serial graph with per-node Linux guardians

Validate all stage resource policies before any writer, execute pending
nodes serially and verify cache hits without new supervisors. Scientific
definitions and synchronous graph APIs are unchanged.

## Usage

``` r
reflow_artifact_plan_run_bounded(plan, directory, supervisor_directory,
  resources, runtime_files, previous = NULL, cancel = function() FALSE)
reflow_artifact_plan_resume_bounded(plan, directory, supervisor_directory,
  resources, runtime_files, cancel = function() FALSE, reconciliations = list())
```

## Arguments

- plan:

  An artifact dependency plan.

- directory:

  Graph generation directory.

- supervisor_directory:

  Separate policy/control root with an existing parent.

- resources:

  Exact named list in canonical stage order; each element has the
  resource fields documented by `reflow_artifact_run_bounded`.

- runtime_files:

  Common named SHA-256 vector covering all writer closures and backend
  files.

- previous:

  Optional completed prior generation for verified in-place reuse.

- cancel:

  Function returning one nonmissing logical cancellation value.

- reconciliations:

  Explicit per-stage artifact retry reconciliations. Never clears
  guardian locks.

## Value

Verified graph descriptor.

## Details

Control policy and attempt journals are separate from graph inventory.
Orphan READY adoption requires a matching successful guardian outcome
and fresh locked verification. Missing journals, failed guardian
outcomes and changed policies fail closed. Ordinary parent unwind
releases its graph/control locks; stale locks after process death
require external inspection. Guardian death is unqualified and requires
external cleanup. Linux only, trusted descendants, per-process address
space, requested threads, and explicit finite deadlines of 1–86400
seconds per node; no graph-wide deadline, parallel scheduler, selector
or targets integration.

## See also

[`reflow_artifact_run_bounded`](https://cttir.github.io/reflowR/reference/reflow_artifact_run_bounded.md),
[`reflow_artifact_plan_run`](https://cttir.github.io/reflowR/reference/reflow_artifact_plan_run.md)
