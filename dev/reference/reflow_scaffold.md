# Plan a Strict Workflow Scaffold

Returns canonical project files without writing or running them. The
supplied dependency lock is parsed data from an existing renv lockfile;
no dependencies are installed or restored. Preserve the returned
declaration outside the generated project as the trusted input to
validation.

## Usage

``` r
reflow_scaffold(profile, name, author, dependency_lock, composition = NULL)
```

## Arguments

- profile:

  A declaration from
  [`reflow_profile()`](https://cttir.github.io/reflowR/dev/reference/reflow_profile.md).

- name:

  Plain-text project name.

- author:

  Plain-text project author.

- dependency_lock:

  Parsed R renv lockfile containing R and Packages fields.

- composition:

  For `strict-cttir-v1` only, a plain list with exactly
  `schema_version = 1L`, `study`, `recipe`, and `runtime`. Study and
  recipe are bounded JSON objects whose scientific semantics are
  validated by cttiR. Runtime has `package = "cttiR"`, `version`,
  `writer = "study_stage"`, and `files`, a complete named list of
  relative installed-package file SHA-256 digests. The package version
  must match the dependency lock. Default `NULL`.

## Value

A list containing the validated profile, trusted declaration, named
relative file contents, directories, and content fingerprints.

## Examples

``` r
# A parsed renv lockfile; in practice use jsonlite::read_json("renv.lock")
pins <- c(digest = "0.6.37", jsonlite = "1.8.9", knitr = "1.49",
          rmarkdown = "2.29", workflowr = "1.7.1")
lock <- list(
  R = list(Version = "4.4.2"),
  Packages = lapply(stats::setNames(names(pins), names(pins)), function(p) {
    list(Package = p, Version = pins[[p]], Source = "Repository")
  })
)
profile <- reflow_profile("clinical", "secondary", "observational")
scaffold <- reflow_scaffold(profile, "Example study", "Your Name", lock)
head(names(scaffold$files))
#> [1] "reflow-profile.json" "renv.lock"           ".Rprofile"          
#> [4] ".gitignore"          "reflow.Rproj"        "code/strict-setup.R"
```
