# Generate Theme CSS from a Color Scheme

Creates the CTTIR v5 site stylesheet for a workflowr (Bootstrap 3) site:
the themakR surface with page, text, heading, link, code and border
colours for light and dark modes, Inter and JetBrains Mono font stacks
without font downloads, and a flat navbar on the page background. The
family accent is used only for identity rules such as the navbar rule,
the title underline, active navigation markers, callout borders and
table header rules.

## Usage

``` r
generate_css(scheme, output_path)
```

## Arguments

- scheme:

  A scheme list from
  [`reflow_scheme()`](https://cttir.github.io/reflowR/dev/reference/reflow_scheme.md).

- output_path:

  Character. File path for the output CSS file.

## Value

Invisibly returns the output path.

## Details

Dark mode follows `prefers-color-scheme` and can be forced with
`data-bs-theme="light"`/`"dark"` or `body.colorscheme-light`/`-dark`.
