# Generate Themed \_site.yml

Creates a `_site.yml` file in the project's `analysis/` directory with
the family icon and project name in the navbar, pipeline menu entries
and the themed
[`workflowr::wflow_html`](https://workflowr.github.io/workflowr/reference/wflow_html.html)
output. The site is written to `docs/`.

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
  [`reflow_scheme()`](https://cttir.github.io/reflowR/reference/reflow_scheme.md).

- preset:

  A preset list from
  [`reflow_preset()`](https://cttir.github.io/reflowR/reference/reflow_preset.md).

- github_url:

  Character or NULL. GitHub URL for navbar link.

- website:

  Character or NULL. Website URL for navbar link.

## Value

Invisibly returns the output file path.
