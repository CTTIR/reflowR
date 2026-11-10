## Submission

This is a new submission: reflowR 0.3.0.

## Test environments

- Local: macOS 27 on Apple silicon, R 4.6.1 (aarch64-apple-darwin),
  `R CMD check --as-cran --no-manual` on the built tarball with `NOT_CRAN`
  unset. The PDF manual was built locally with TinyTeX (TeX Live 2026) via
  `R CMD Rd2pdf` without errors; the check run itself used `--no-manual`.
- GitHub Actions (r-lib/actions v2, `--as-cran`): macOS-latest (R release),
  windows-latest (R release), ubuntu-latest (R devel, R release, R oldrel-1),
  plus a separate minimum-R 4.1 workflow.

## R CMD check results

0 errors | 0 warnings | 1 note

- New submission.

With `NOT_CRAN` unset the tests take about two minutes on Linux; they
generate and build complete 'workflowr' projects in temporary directories.

## Examples and run time

- The examples of `reflow_artifact_run()`, `reflow_artifact_plan_run()`,
  `reflow_artifact_select()` and `reflow_artifact_selection_verify()`
  execute a pipeline, which fingerprints every installed package the stages
  use (tens of seconds of CPU time). Their running, selecting and resuming
  parts are in `\donttest{}`; for the first two, constructing the
  specifications still runs.
- The bounded-execution examples need Linux process limits and are in
  `\donttest{}` as well.

## Code and operational boundaries

- `reflow_init(change_wd = TRUE)` intentionally changes the working
  directory, as `workflowr::wflow_start()` does; the default is `TRUE` only in
  interactive sessions. Examples and tests use `change_wd = FALSE` and write
  only below `tempdir()`.
- Generated project files install nothing automatically.
- Stochastic stages require an explicit seed and restore the caller's RNG
  kind and `.Random.seed`.
- Tests that install fixture packages, run long recovery scenarios, run the
  slowest complete artifact graphs or need Linux process supervision are
  skipped on CRAN.

## Downstream dependencies

There are no reverse dependencies on CRAN.
