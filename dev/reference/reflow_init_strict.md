# Write a Strict Workflow Project

Writes a new project from canonical templates, without installing
dependencies, committing, rendering or publishing. Existing paths and
symbolic links are rejected. Raw data files and their directory are made
read-only.

## Usage

``` r
reflow_init_strict(
  directory,
  profile,
  name,
  author,
  dependency_lock,
  git = FALSE,
  composition = NULL
)
```

## Arguments

- directory:

  New project directory.

- profile:

  A declaration from
  [`reflow_profile()`](https://cttir.github.io/reflowR/dev/reference/reflow_profile.md).

- name:

  Plain-text project name.

- author:

  Plain-text project author.

- dependency_lock:

  Parsed R renv lockfile containing R and Packages fields.

- git:

  Whether to initialize an empty Git repository. Default `FALSE`. No
  commit is made and Git identity configuration is not changed.

- composition:

  For `strict-cttir-v1` only, a plain list with exactly
  `schema_version = 1L`, `study`, `recipe`, and `runtime`. Study and
  recipe are bounded JSON objects whose scientific semantics are
  validated by cttiR. Runtime has `package = "cttiR"`, `version`,
  `writer = "study_stage"`, and `files`, a complete named list of
  relative installed-package file SHA-256 digests. The package version
  must match the dependency lock. Default `NULL`.

## Value

Invisibly, the scaffold with its absolute project directory.

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
project <- tempfile("strict-", tmpdir = normalizePath(tempdir(), mustWork = TRUE))
scaffold <- reflow_init_strict(project, profile, "Example study", "Your Name", lock)
list.files(project)
#>  [1] "README.md"           "_workflowr.yml"      "analysis"           
#>  [4] "code"                "data"                "docs"               
#>  [7] "output"              "publications"        "reflow-profile.json"
#> [10] "reflow.Rproj"        "renv"                "renv.lock"          

# Raw data are read-only; make them writable again before deleting
Sys.chmod(file.path(project, "data", "raw"), "0755")
unlink(project, recursive = TRUE)
```
