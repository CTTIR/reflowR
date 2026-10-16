# Validate a Strict Project Against an External Declaration

Reads files without executing project code. The declaration and reviewed
hashes must come from a trusted location outside the generated project.
A project file cannot approve itself. Static validity, execution
readiness and recorded completion are separate results; none constitutes
scientific or legal approval.

## Usage

``` r
reflow_validate_strict(
  directory,
  declaration,
  reviewed_files = NULL,
  execution_receipts = NULL
)
```

## Arguments

- directory:

  Existing strict project directory.

- declaration:

  The `declaration` returned by
  [`reflow_scaffold()`](https://cttir.github.io/reflowR/dev/reference/reflow_scaffold.md),
  retained externally. Contains profile, name, author, dependency_lock
  and, only for `strict-cttir-v1`, the exact composition.

- reviewed_files:

  Optional named character vector mapping declared stage Rmd/R paths to
  externally reviewed SHA-256 hashes. Control files are immutable. The
  composed cttiR profile refuses all reviewed-file overrides.

- execution_receipts:

  Optional named character vector mapping stage IDs to externally
  retained SHA-256 hashes of their execution receipts.

## Value

A list with `valid`, `pristine`, `execution_ready`, `completed`,
per-stage results and diagnostic checks. Structural errors return
`valid=FALSE`.
