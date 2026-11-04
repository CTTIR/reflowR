## Status: incomplete qualification; no submission

Version 0.2.1 is a reviewed source checkpoint. On 4 November 2026 the user stopped
audit/refinement to continue on another machine, then requested repository work
be synchronized to main. Source publication is not CRAN release qualification.
No submission or acceptance is claimed. The detailed continuation record is
`planning/maintenance-2026-11-04.md`.

## Actual environments and results

- Baseline 0.2.0: macOS ARM, R 4.6.1, `R CMD check --as-cran` completed with
  0 errors, 0 warnings and 1 new-submission NOTE; archive tests passed 3,655
  assertions with 62 explicit skips. Baseline coverage was 89.381%.
- Candidate1 0.2.1 archive SHA-256:
  `4496331f4126b84a90ff14ac1ec75f338cd379a98765dfbadd29e0762b0f7c5d`.
  Full and required-dependency checks were started but stopped by user request.
  The minimal check completed both example passes and was stopped during tests;
  its resulting SIGTERM/test ERROR is an interruption, not a completed result.
  No full candidate check pass is claimed.
- Candidate1 actually built and locally published all 11 standard workflowr pages,
  preserving cwd and verified Git identity, without a remote. Required-dependency
  first-use passed 19 checks; a separate `here`-only layer passed 7 generated-setup
  and checkpoint checks. Optional analysis/table packages stayed absent there.
- Candidate2 archive SHA-256:
  `96503c893d820fc55cdf52216fedfbb6dd0561dd3e14c4c6e55778996b72c971`.
  It changes only console command output, its regression test and generated
  DESCRIPTION timestamp. The source-level preservation/command suite passed 126
  assertions. Final candidate2 full/minimal checks and coverage did not complete.
- The Linux container stopped during installation; guardian lifecycle and Linux
  installed suites had not started. Native Windows and the added minimum-R 4.1
  workflow have no observed final-version result. Final site/manual review is
  also incomplete. These are explicit outstanding gates.

Targeted source and installed-snapshot tests have separate evidence and scopes;
none substitutes for the interrupted full checks. The 85% coverage gate is now
configured, but only baseline coverage was measured to completion. The historical
house lint convention scan remains report-only without new suppressions.

## Observed timing NOTE

The repaired selected runtime closure measured 68 packages, 4,395 files and
146,896,040 bytes in one local inventory. Full default uncached verification
reads these resources again. Both candidate1 check environments observed an
example timing NOTE: approximately 63–66 CPU seconds for the two-stage artifact
graph, 16 for a single artifact and 4 for imaging. Concurrent work increases
elapsed time but does not erase the CPU cost.

The read-only tools/digest comparison returned identical SHA-256 values with no
material CPU advantage. Defaults and checks were retained. The existing opt-in
`options(reflowR.package_hash_cache = TRUE)` reuses hashes within a process while
file metadata remains unchanged; this is a distinct tradeoff, not the check
configuration. Review performance and example timing before submission without
weakening provenance or acceptance tests. No clean-CRAN claim is made.

## Code and operational boundaries

- `reflow_init(change_wd = TRUE)` intentionally changes cwd; its default is TRUE
  only interactively. Examples/tests use FALSE and explicit project-qualified
  workflowr commands. Scaffold recovery is synchronous, not crash-atomic or a
  concurrent-writer protocol; retained-backup errors require inspection.
- The generated setup installs nothing automatically. `here` is an explicit
  generated-project prerequisite; optional table helpers require gtsummary >= 2.0.0
  and therefore R >= 4.2. Core declared support remains R >= 4.1, pending the new
  actual minimum-version check.
- Stochastic stages require an explicit seed and restore the caller's RNG kind
  and `.Random.seed`. Inputs must remain immutable during execution.
- `find.package()` selects runtime package locations before recursive
  Depends/Imports traversal. Selected installed code/resources are fingerprinted;
  optional dynamic packages and external runtime files remain explicit declarations.
- Bounded guardian examples require supported Linux, processx/ps, prlimit/nice,
  suitable runtime pins, an unlinked library and the documented resource limits.
  macOS skips do not qualify Linux supervision. Fixture-install/long recovery
  tests retain their existing CRAN skips, not newly introduced suppressions.

## Before any submission

Complete exact-archive full/minimal checks, installed tests, coverage, native
Windows and minimum-R checks; qualify Linux supervision separately; finish the
manual/site review and fresh reverse-dependency inventory. Reassess example
runtime. No current downstream inventory, external acceptance or deployment is
claimed. This file is a handoff, not a completed submission report.
