# Generate about.Rmd

Generate about.Rmd

## Usage

``` r
generate_about_rmd(
  project_dir,
  name,
  author,
  email,
  affiliation,
  orcid,
  website,
  scheme,
  preset
)
```

## Arguments

- project_dir:

  Character. Path to project root.

- name:

  Character. Project name.

- author:

  Character. Author name.

- email:

  Character or NULL. Author email.

- affiliation:

  Character or NULL. Affiliation.

- orcid:

  Character or NULL. ORCID iD.

- website:

  Character or NULL. Website URL.

- scheme:

  A scheme list.

- preset:

  A preset list.

## Value

Invisibly returns the output file path.
