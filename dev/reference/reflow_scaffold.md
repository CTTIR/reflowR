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
