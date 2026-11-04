# Maintenance handoff — 4 November 2026

The audit started from `5ce581f5e96696f336081a2c2360bbd528f58dad`
(version 0.2.0). The user subsequently stopped audit/refinement, to continue it
on another machine, and explicitly requested that local repository work be
synchronized to main. The version 0.2.1 changes below are a source checkpoint.
**Final release qualification is incomplete.** A normal source push does not
constitute CRAN submission, acceptance, deployment or scientific validation.

The original checkout and its data were preserved during the audit. Changes
were developed in an isolated worktree. Existing APIs, author metadata, artwork,
record schemas and the trusted-code/non-sandbox boundary remain in place.

## Implemented checkpoint

Five commits contain the reviewed implementation and documentation work:

| Commit | Change |
| --- | --- |
| `09a3232f4d62f6a89c5facb10648516e39a9c308` | Validate scaffold arguments, generate privately before publication, refuse managed links and changed destinations, restore prior files/modes/index on supported failures, retain named recovery backups when restoration fails. |
| `19a7c6198595316a6bc1feafa52c6e4d2eafa22c` | Align paired comparisons by explicit complete pair IDs, preserve missing-data semantics, match checkpoint names literally, refuse checkpoint collisions and support optional table packages. |
| `e812962d1e76f2549809d274b50837d814a19304` | Fingerprint empty-directory layout, actual linked targets and selected loaded dependency closures; refuse special files and directory cycles; use portable link checks for artifact/strict boundaries. |
| `d43ebf2a6b39cb87fc99f3f2998f55739c66854f` | Print complete project-qualified build/publish commands verbatim, preserving spaces, braces and non-ASCII paths at narrow console widths. |
| `81009c8c004efb04cf91885b75faa662aa0227b0` | Document runnable first use and helper contracts; add an 85% coverage gate and a minimum-R workflow; retain the separately classified report-only house lint scan. |

The repairs address reproduced failures: late scaffold generation changed six
existing files; linked outputs changed an external sentinel; invalid scalar
metadata was accepted; stale results survived directory/layout, linked-target
or loaded-dependency changes; FIFO hashing blocked; and generated paired tests
used an unsupported formula call. Failure fixtures and original logs are
retained outside the source tree.

Scaffold and checkpoint recovery is synchronous, not crash-atomic or protection
against arbitrary concurrent writers. ACLs and ownership are not copied.
Imaging inputs must remain immutable during execution. Optional packages and
external runtime files must still be declared explicitly. These limits were not
relaxed to obtain passing tests.

## What was actually verified

These are separate scopes; overlapping test counts must not be added into a
single package-wide result.

- The immutable 0.2.0 baseline passed macOS ARM R 4.6.1
  `R CMD check --as-cran`: 0 errors, 0 warnings, 1 new-submission NOTE.
  Archive tests passed 3,655 assertions with 62 explicit skips. The separate
  installed/source-test-directory run passed 3,722 assertions with 59 skips.
- Baseline coverage was 89.381%. This supports introducing the 85% gate; it is
  **not** a measurement of final version 0.2.1.
- Final scaffold/console source tests passed 126 new assertions. Existing init
  tests passed 121 assertions with one pre-existing global-Git-identity skip;
  seven generator suites passed 806 assertions. All 28 ordinary scaffold file
  payloads matched baseline when using the same baseline generators.
- Paired-comparison, checkpoint-loader and existing generator checks passed
  79 assertions. Eight synthetic presentation checks preserved supplied values,
  coordinates, category colours and scientific payload identities.
- An immutable installed provenance snapshot passed 425 affected assertions
  without failures, errors, warnings or skips, with package hash caching enabled.
  Its relevant source/resource hashes match candidate2. Separate actual
  dual-library, linked-input, FIFO and cycle probes establish the repaired
  boundaries; this is not a full installed-package suite result.
- Candidate1 built and locally published all 11 standard workflowr pages from
  an outside working directory. The working directory and verified maintainer
  Git identity were preserved; the fixture had no remote.
- Candidate1 installed into a required-dependency library with 75 runtime/testthat
  packages, an explicit empty site library and R base. Effectsize, gtsummary,
  here, purrr, tibble and targets were absent. Core first-use passed 19 checks:
  scaffold/configuration, refusal preservation, imaging execution/readback,
  unchanged resume, changed-input rejection and corrupt-result rejection.
  A separate layer adding only `here` passed seven generated-setup/checkpoint
  checks, while optional analysis/table packages remained absent.
- The candidate1 documentation site completed with 59 HTML pages and no broken
  links among 2,389 HTML and 54 CSS references. Provisional theme/code contrast
  review and source preservation checks passed. This is not final candidate2
  browser/manual qualification.

## Exact artifacts and stopped work

| Archive | SHA-256 | Qualification boundary |
| --- | --- | --- |
| Candidate1, `reflowR_0.2.1.tar.gz` | `4496331f4126b84a90ff14ac1ec75f338cd379a98765dfbadd29e0762b0f7c5d` | Installed first-use and partial full/minimal checks described above. |
| Candidate2, `reflowR_0.2.1.tar.gz` | `96503c893d820fc55cdf52216fedfbb6dd0561dd3e14c4c6e55778996b72c971` | Exact delta: initializer console output, its regression test and generated DESCRIPTION timestamp. No completed full/minimal check. |

At the user's stop instruction on 4 November 2026, 16:16 UTC:

- The full candidate1 check had not completed. The minimal candidate1 check
  completed code/documentation checks and both example passes, then was stopped
  during installed tests. Its SIGTERM-related test ERROR is an intentional
  interruption, not a completed integration failure or a passing check.
- Candidate2 coverage had installed the instrumented package and entered tests,
  but produced no final coverage result. An earlier launcher-path error is also
  retained. Neither result is replaced with the baseline percentage.
- Candidate2's minimal dependency library and exact-delta record were prepared;
  no package installation or tests were started in that library.
- The owned Linux container was stopped during baseline installation. Guardian
  lifecycle and Linux installed suites had **not started**. The reviewed
  synthetic success/error/timeout/cancellation/guardian-loss harness is prepared
  only; dependency availability is not process-supervision qualification.
- Candidate2's targeted documentation rebuild stopped at search-index generation.
  Final site/browser/link/asset closeout and manual PDF inspection remain undone.
- No final-version native Windows or minimum-R result was observed. The added
  R 4.1 workflow is configuration, not evidence that it has passed.

All logs, fixtures, source snapshots and stop receipts remain in the local audit
evidence directory (`evidence/reflowR` in the dated repository audit workspace).
Key records are `scaffold/verification.json`, `minimal/STOPPED-HANDOFF.json`,
`provenance/REVIEW.json`, `provenance/linux/STOPPED-HANDOVER.json`,
`presentation/STOP-HANDOFF.md`, the candidate build receipts and first-use results.

## Runtime cost and remaining gates

The repaired closure is deliberately complete: one local measurement contained
68 packages, 4,395 files and 146,896,040 bytes. A single uncached signature took
about 3.2 CPU seconds; repeated verification accumulates that cost. The full and
minimal candidate1 checks recorded real slow-example NOTEs: roughly 63–66 CPU
seconds for the two-stage artifact graph, about 16 for one artifact, and about
4 for imaging. Concurrent jobs increased wall time but do not explain away the
CPU cost. Both digest and tools produced identical SHA-256 values with no
material backend speed advantage.

Default full hashing was retained. The existing opt-in
`options(reflowR.package_hash_cache = TRUE)` trades repeated reads for
within-process metadata validation; it is not the setting used by those checks.
Performance and CRAN example-time review remain open. The 497 classified house
lint findings (473 condition conventions, 24 vocabulary conventions) also remain
report-only: a separate compatibility-aware migration is required rather than
suppressing checks or mechanically changing standalone worker semantics.

## Continue on the next machine

1. Fresh-pull the source checkpoint into an isolated clean checkout. Record the
   exact commit, R/dependency versions and newly built archive hash. Retain the
   earlier failure and stop evidence; do not treat partial libraries as qualified.
2. Complete the final archive's full `R CMD check --as-cran`, installed tests and
   required-dependency check with an explicitly empty site library. Run final
   coverage and verify the 85% gate; inspect native Windows and R 4.1 results.
3. Repeat the named first-use paths below. Review the uncached runtime cost
   without omitting dependency/resource identity or weakening acceptance tests.
4. On supported Linux, finish installation and run the reviewed owned-fixture
   guardian cases, checking actual process identities, expected status/reason,
   descendant cleanup and retained uncertain-state locks.
5. Finish the final site/manual build, link and responsive/theme review, and
   scientific figure preservation checks. Only then reassess submission/readiness.
   No CRAN submission, release or deployment is authorized by this handoff.

Core prerequisites are R >= 4.1 and the declared package dependencies. Generated
projects additionally require `here`; optional gtsummary helpers require
R >= 4.2. HTML rendering needs Pandoc, supplied by RStudio or installed separately.
Local publishing needs configured maintainer Git name/email; it does not push.

```r
if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
pak::pak(c("CTTIR/reflowR", "here"))
stopifnot(rmarkdown::pandoc_available())
project <- reflowR::reflow_init(
  file.path(tempdir(), "first-analysis"), depth = "standard",
  change_wd = FALSE, open = FALSE
)
workflowr::wflow_build(project = project)
workflowr::wflow_publish(file.path(project, "analysis", "*.Rmd"),
                        message = "Build reviewed analysis", project = project)
```

Use a fresh destination, review generated analysis before local publication,
and choose a persistent directory for real work. The generated setup reports
missing optional packages; it does not install them automatically.
