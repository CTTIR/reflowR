# Generate Themed \_site.yml

Creates a `_site.yml` file in the project's `analysis/` directory with
themed navbar, icon, and pipeline menu entries.

## Usage

``` r
generate_site_yml(
  project_dir,
  name,
  scheme,
  preset,
  github_url = NULL,
  website = NULL
)
```

## Arguments

- project_dir:

  Character. Path to the project root.

- name:

  Character. Project name.

- scheme:

  A scheme list from
  [`reflow_scheme()`](https://r-heller.github.io/reflowR/reference/reflow_scheme.md).

- preset:

  A preset list from
  [`reflow_preset()`](https://r-heller.github.io/reflowR/reference/reflow_preset.md).

- github_url:

  Character or NULL. GitHub URL for navbar link.

- website:

  Character or NULL. Website URL for navbar link.

## Value

Invisibly returns the output file path.
