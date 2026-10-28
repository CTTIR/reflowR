# Get a Color Scheme Definition

Returns the site scheme for one CTTIR v5 topic family. Every colour,
surface and font is derived from `inst/cttir/design_tokens.csv`, the
token table shared with hexmakR and the themakR documentation template.

## Usage

``` r
reflow_scheme(name)
```

## Arguments

- name:

  Character. One of the v5 families `"spectral"`, `"spatial"`,
  `"omics"`, `"clinical"`, `"methods"`, `"simulation"`, `"evidence"`,
  `"platform"` or `"brand"`. The earlier names remain accepted as
  aliases: `"basic"` (brand), `"code"` (methods), `"special"` (evidence)
  and `"other"` (platform). An alias prints a one-time message per
  session.

## Value

A named list. `family`, `label`, `accent` and `navbar_icon` identify the
family. Surfaces are given per colour mode as `page_*`, `text_*`,
`heading_*`, `link_*`, `code_surface_*`, `border_*` and `muted_*` with
suffix `_light` or `_dark`; `face_light` and `face_dark` are the tinted
tile faces, `syntax_light` and `syntax_dark` the code token colours,
`palette` the Okabe-Ito topic colours starting with the family accent,
and `font_body`, `font_code`, `font_brand` and `brand_weight` the
typography. The earlier fields (`primary`, `primary_light`, `secondary`,
`light_grey`, `grey`, `dark`, `white`, `gradient_from`, `gradient_to`
and the named hues) remain available and are derived from the same
tokens.

## See also

[`reflow_schemes()`](https://cttir.github.io/reflowR/dev/reference/reflow_schemes.md)
for listing all available schemes.

## Examples

``` r
s <- reflow_scheme("clinical")
s$accent
#> [1] "#D55E00"
s$palette
#> [1] "#D55E00" "#0072B2" "#009E73" "#E69F00" "#CC79A7" "#56B4E9" "#000000"
#> [8] "#F0E442"
```
