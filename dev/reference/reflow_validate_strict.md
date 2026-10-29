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

## Examples

``` r
pins <- c(digest = "0.6.37", jsonlite = "1.8.9", knitr = "1.49",
          rmarkdown = "2.29", workflowr = "1.7.1")
lock <- list(
  R = list(Version = "4.4.2"),
  Packages = lapply(stats::setNames(names(pins), names(pins)), function(p) {
    list(Package = p, Version = pins[[p]], Source = "Repository")
  })
)
profile <- reflow_profile("clinical", "secondary", "observational")
project <- tempfile("strict-", tmpdir = normalizePath(tempdir(), winslash = "/", mustWork = TRUE))
scaffold <- reflow_init_strict(project, profile, "Example study", "Your Name", lock)

# Keep the declaration outside the project and validate against it
result <- reflow_validate_strict(project, scaffold$declaration)
result[c("valid", "pristine", "execution_ready", "completed")]
#> $valid
#> [1] TRUE
#> 
#> $pristine
#> [1] TRUE
#> 
#> $execution_ready
#> [1] FALSE
#> 
#> $completed
#> [1] FALSE
#> 

Sys.chmod(file.path(project, "data", "raw"), "0755")
unlink(project, recursive = TRUE)
```
