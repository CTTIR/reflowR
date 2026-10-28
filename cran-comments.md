## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.
* "Version contains large components (0.1.0.9000)": the development version
  number will be replaced by a release version before submission.

## Test environments

* local: Ubuntu 26.04, R 4.6.1 (`R CMD check --as-cran`, also with
  `_R_CHECK_DEPENDS_ONLY_=true`)
* GitHub Actions: the repository's R-CMD-check workflow, with ubuntu-latest
  (R devel, release, oldrel-1), macOS-latest (R release) and windows-latest
  (R release). It will be run on the submitted commit.

## Notes on the code

* `reflow_init(change_wd = TRUE)` calls `setwd()` to move into the newly created
  project, as `workflowr::wflow_start(change_wd = TRUE)` does. Changing the
  directory is the documented purpose of this argument, so it is not reset on
  exit. It defaults to `TRUE` only in interactive sessions; the examples, tests
  and vignettes use `change_wd = FALSE`.
* The imaging and artifact runners (`reflow_imaging_run()`,
  `reflow_artifact_run()` and their graph versions) call stochastic stages
  with an explicit stage seed and afterwards restore the caller's RNG kind and
  `.Random.seed` (or remove it if it did not exist), so the user's random
  number stream is left unchanged.
* `utils::installed.packages()` is used only to compute the recursive
  Depends/Imports closure of the packages whose installed files are
  fingerprinted in artifact and imaging receipts. It does not test whether a
  package is installed, and nothing is installed or downloaded.
* The generated project setup script does not install packages; it attaches
  the installed ones and lists missing ones in a message.
* The examples of `reflow_artifact_run_bounded()` and
  `reflow_artifact_plan_run_bounded()` are in `\donttest{}`: they start
  subprocesses and hash every installed file of the package closure (20 to
  40 seconds). They also run only on Linux with 'processx', 'ps', `prlimit`
  and `nice` available, a package library without symbolic links (which the
  guardian requires), and a process priority of at most 10 and an
  address-space limit of at least 8 GiB (which the examples request);
  otherwise they do nothing. They restore the thread environment variables
  they set, also when a run fails.
* Tests that install fixture packages with `R CMD INSTALL` and the long
  recovery scenarios of the artifact selector are skipped on CRAN; the test
  suite takes about 40 seconds there.

## Downstream dependencies

This is a new package with no downstream dependencies.
