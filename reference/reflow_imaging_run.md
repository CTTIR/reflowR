# Execute or resume a recorded imaging plan

Runs are sequential and local. Every call result is saved to a new
immutable attempt directory. Receipts contain configuration, input,
package, runtime, resolved-argument and output hashes. Resume rejects
changed definitions or inputs and corrupt results, rather than silently
reusing stale output. A failed call can be retried with the identical
plan; completed stages are verified before reuse. Random stages require
an explicit stage seed; the caller's random state is restored after each
call.

## Usage

``` r
reflow_imaging_run(plan, directory)

reflow_imaging_resume(plan, directory)
```

## Arguments

- plan:

  A
  [`reflow_imaging_plan()`](https://cttir.github.io/reflowR/reference/reflow_imaging_plan.md).

- directory:

  New run directory, or existing run directory for resume.

## Value

Invisibly, a named list of stage result objects.

## Details

The executor never initializes Git, publishes, installs packages or
downloads data. Called functions are trusted and can have side effects;
this API does not sandbox them. File-producing, distributed and
resource-scheduled stages are outside this initial object-returning
contract. Package fingerprints cover installed code, native libraries
and dependency metadata, not external engines.

A directory lock prevents concurrent execution. After an interrupted
process, inspect the lock's owner receipt and verify that process has
stopped before manually removing `.lock`; stale locks are never
automatically stolen.
