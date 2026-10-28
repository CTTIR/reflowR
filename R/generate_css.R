#' Generate Theme CSS from a Color Scheme
#'
#' Creates the CTTIR v5 site stylesheet for a workflowr (Bootstrap 3) site:
#' the themakR surface with page, text, heading, link, code and border colours
#' for light and dark modes, Inter and JetBrains Mono font stacks without font
#' downloads, and a flat navbar on the page background. The family accent is
#' used only for identity rules such as the navbar rule, the title underline,
#' active navigation markers, callout borders and table header rules.
#'
#' Dark mode follows `prefers-color-scheme` and can be forced with
#' `data-bs-theme="light"`/`"dark"` or `body.colorscheme-light`/`-dark`.
#'
#' @param scheme A scheme list from [reflow_scheme()].
#' @param output_path Character. File path for the output CSS file.
#'
#' @return Invisibly returns the output path.
#'
#' @keywords internal
generate_css <- function(scheme, output_path) {
  writeLines(site_css(scheme), output_path)
  invisible(output_path)
}

# Custom properties for one colour mode, named as in themakR's
# _cttir-tokens.scss and extended by muted text, tile face and code tokens.
css_mode_properties <- function(s, mode) {
  field <- function(name) s[[paste0(name, "_", mode)]]
  syntax <- field("syntax")
  c(sprintf("  color-scheme: %s;", mode),
    sprintf("  --cttir-page: %s;", field("page")),
    sprintf("  --cttir-text: %s;", field("text")),
    sprintf("  --cttir-heading: %s;", field("heading")),
    sprintf("  --cttir-link: %s;", field("link")),
    sprintf("  --cttir-code-surface: %s;", field("code_surface")),
    sprintf("  --cttir-border: %s;", field("border")),
    sprintf("  --cttir-muted: %s;", field("muted")),
    sprintf("  --cttir-face: %s;", field("face")),
    sprintf("  --cttir-show-light: %s;", if (mode == "light") "block" else "none"),
    sprintf("  --cttir-show-dark: %s;", if (mode == "dark") "block" else "none"),
    sprintf("  --cttir-syntax-%s: %s;", names(syntax), syntax))
}

# Token blocks shared by the CSS and SCSS generators. Explicit attributes and
# body classes override the system preference, as in themakR; print always
# uses the light tokens.
css_token_blocks <- function(s) {
  light <- css_mode_properties(s, "light")
  dark <- css_mode_properties(s, "dark")
  accents <- vapply(.cttir_schemes(), `[[`, character(1), "accent")
  paste(c(
    ":root {",
    sprintf('  --cttir-font-body: "%s", system-ui, sans-serif;', s$font_body),
    sprintf('  --cttir-font-code: "%s", ui-monospace, monospace;', s$font_code),
    sprintf('  --cttir-font-brand: "%s", ui-monospace, monospace;', s$font_brand),
    sprintf("  --cttir-brand-weight: %s;", s$brand_weight),
    sprintf("  --cttir-topic: %s;", s$accent),
    "}",
    ":root {", light, "}",
    "@media (prefers-color-scheme: dark) {", ":root {", dark, "}", "}",
    '[data-bs-theme="light"], body.colorscheme-light {', light, "}",
    '[data-bs-theme="dark"], body.colorscheme-dark {', dark, "}",
    "@media (prefers-color-scheme: dark) {", "body.colorscheme-auto {", dark, "}", "}",
    "@media print {",
    ':root, [data-bs-theme="dark"], body.colorscheme-dark, body.colorscheme-auto {',
    light, "}", "}",
    sprintf('[data-cttir-topic="%s"] { --cttir-topic: %s; }', names(accents), accents)
  ), collapse = "\n")
}

# Pandoc highlighting classes grouped by token colour. Prefixing body outranks
# the highlight style embedded by rmarkdown, in both colour modes.
css_syntax_rules <- function() {
  groups <- list(keyword = c("kw", "cf", "im", "pp", "ex", "dt"),
                 string = c("st", "ch", "vs", "ss", "sc"),
                 number = c("dv", "bn", "fl", "cn"),
                 "function" = c("fu", "bu", "at"),
                 comment = c("co", "do", "an", "cv", "in"),
                 alert = c("al", "er", "wa"),
                 text = c("va", "ot", "op"))
  rules <- vapply(names(groups), function(g) {
    selectors <- paste0("body code span.", groups[[g]], collapse = ", ")
    colour <- if (g == "text") "var(--cttir-text)" else paste0("var(--cttir-syntax-", g, ")")
    style <- if (g == "comment") "; font-style: italic" else ""
    paste0(selectors, " { color: ", colour, style, "; }")
  }, character(1))
  paste(rules, collapse = "\n")
}

site_css <- function(scheme) {
  s <- scheme
  rules <- glue::glue('
/* --- Page --- */
body {
  background-color: var(--cttir-page);
  color: var(--cttir-text);
  font-family: var(--cttir-font-body);
  line-height: 1.6;
}
::selection {
  background-color: var(--cttir-face);
  color: var(--cttir-heading);
}
hr {
  border-top-color: var(--cttir-border);
}
.text-muted, .help-block, .figure .caption, p.caption, figcaption, caption {
  color: var(--cttir-muted);
}
.figure .caption, p.caption, figcaption {
  font-style: italic;
  margin-top: 0.5em;
  text-align: center;
}

/* --- Headings --- */
h1, h2, h3, h4, h5, h6, .h1, .h2, .h3, .h4, .h5, .h6 {
  color: var(--cttir-heading);
  font-family: var(--cttir-font-body);
  font-weight: 600;
}
h1, h1.title {
  border-bottom: 3px solid var(--cttir-topic);
  padding-bottom: 0.3em;
}
h2 {
  border-bottom: 1px solid var(--cttir-border);
  padding-bottom: 0.25em;
}
h4.author, h4.date {
  color: var(--cttir-muted);
  font-weight: 400;
}

/* --- Links: the link token, never the topic accent --- */
a, a:visited {
  color: var(--cttir-link);
}
a:hover, a:focus {
  color: var(--cttir-link);
  text-decoration: underline;
}

/* --- Navbar: flat, on the page background, accent rule as identity cue --- */
.navbar, .navbar-default {
  background: var(--cttir-page);
  background-image: none;
  border: 0;
  border-radius: 0;
  box-shadow: inset 0 -3px 0 var(--cttir-topic);
}
.navbar-default .navbar-brand,
.navbar-default .navbar-brand:hover,
.navbar-default .navbar-brand:focus {
  color: var(--cttir-heading);
  font-family: var(--cttir-font-brand);
  font-weight: var(--cttir-brand-weight);
  letter-spacing: -0.02em;
}
.navbar-brand {
  display: inline-flex;
  align-items: center;
  gap: 0.5rem;
  padding-top: 10px;
  padding-bottom: 10px;
}
.navbar-brand img {
  display: inline-block;
  height: 30px;
  width: auto;
}
.navbar-default .navbar-nav > li > a {
  color: var(--cttir-heading);
}
.navbar-default .navbar-nav > li > a:hover,
.navbar-default .navbar-nav > li > a:focus {
  color: var(--cttir-heading);
  background-color: var(--cttir-code-surface);
}
.navbar-default .navbar-nav > .active > a,
.navbar-default .navbar-nav > .active > a:hover,
.navbar-default .navbar-nav > .active > a:focus,
.navbar-default .navbar-nav > .open > a,
.navbar-default .navbar-nav > .open > a:hover,
.navbar-default .navbar-nav > .open > a:focus {
  color: var(--cttir-heading);
  background-color: var(--cttir-code-surface);
  box-shadow: inset 0 -3px 0 var(--cttir-topic);
}
.navbar-default .navbar-toggle {
  border-color: var(--cttir-border);
}
.navbar-default .navbar-toggle:hover,
.navbar-default .navbar-toggle:focus {
  background-color: var(--cttir-code-surface);
}
.navbar-default .navbar-toggle .icon-bar {
  background-color: var(--cttir-heading);
}
.navbar-default .navbar-collapse, .navbar-default .navbar-form {
  border-color: var(--cttir-border);
}

/* --- Dropdown menus: text stays on neutral surfaces --- */
.dropdown-menu {
  background-color: var(--cttir-page);
  border: 1px solid var(--cttir-border);
  border-radius: 4px;
  box-shadow: none;
}
.dropdown-menu > li > a,
.navbar-default .navbar-nav .open .dropdown-menu > li > a {
  color: var(--cttir-text);
  padding: 6px 20px;
}
.dropdown-menu > li > a:hover,
.dropdown-menu > li > a:focus,
.navbar-default .navbar-nav .open .dropdown-menu > li > a:hover,
.navbar-default .navbar-nav .open .dropdown-menu > li > a:focus {
  color: var(--cttir-heading);
  background-color: var(--cttir-code-surface);
}
.dropdown-menu > .active > a,
.dropdown-menu > .active > a:hover,
.dropdown-menu > .active > a:focus,
.navbar-default .navbar-nav .open .dropdown-menu > .active > a {
  color: var(--cttir-heading);
  background-color: var(--cttir-code-surface);
  box-shadow: inset 3px 0 0 var(--cttir-topic);
}
.dropdown-menu .divider {
  background-color: var(--cttir-border);
}
.dropdown-header {
  color: var(--cttir-muted);
}

/* --- Table of contents (tocify) --- */
#TOC, .tocify {
  background-color: var(--cttir-page);
  border: 1px solid var(--cttir-border);
  border-radius: 4px;
}
.tocify .list-group-item, .tocify-item {
  background-color: transparent;
  border-color: var(--cttir-border);
  color: var(--cttir-text);
}
.tocify .list-group-item:hover, .tocify-item:hover {
  background-color: var(--cttir-code-surface);
  color: var(--cttir-heading);
}
.tocify .list-group-item.active,
.tocify .list-group-item.active:hover,
.tocify .list-group-item.active:focus,
.tocify-item.active {
  background-color: var(--cttir-code-surface);
  border-color: var(--cttir-border);
  color: var(--cttir-heading);
  box-shadow: inset 3px 0 0 var(--cttir-topic);
}
.tocify-item.active a {
  color: var(--cttir-heading);
}

/* --- Code --- */
code, kbd, pre, samp {
  font-family: var(--cttir-font-code);
}
body pre, body pre:not([class]), body div.sourceCode, body pre.sourceCode {
  background-color: var(--cttir-code-surface);
  border: 1px solid var(--cttir-border);
  border-radius: 4px;
  color: var(--cttir-text);
}
body pre {
  padding: 12px 16px;
  font-size: 0.875em;
}
body div.sourceCode pre.sourceCode {
  border: 0;
  margin: 0;
}
:not(pre) > code {
  background-color: var(--cttir-code-surface);
  color: var(--cttir-text);
  padding: 0.125rem 0.3125rem;
  border-radius: 3px;
  font-size: 0.875em;
}
pre code, body pre > code.sourceCode {
  background-color: transparent;
  color: inherit;
  padding: 0;
}
<<css_syntax_rules()>>

/* --- Buttons and code folding --- */
.btn-default, .btn-default.btn-xs, .code-folding-btn {
  background-color: var(--cttir-page);
  border-color: var(--cttir-border);
  color: var(--cttir-text);
}
.btn-default:hover, .btn-default:focus, .btn-default:active,
.btn-default.active, .open > .dropdown-toggle.btn-default {
  background-color: var(--cttir-code-surface);
  border-color: var(--cttir-border);
  color: var(--cttir-heading);
}
.btn-primary, .btn-primary:hover, .btn-primary:focus, .btn-primary:active {
  background-color: var(--cttir-heading);
  border-color: var(--cttir-heading);
  color: var(--cttir-page);
}

/* --- Tables --- */
table, .table {
  color: var(--cttir-text);
  border-collapse: collapse;
  margin-bottom: 1.5rem;
}
thead th, .table > thead > tr > th {
  background-color: transparent;
  color: var(--cttir-heading);
  font-family: var(--cttir-font-code);
  font-size: 0.875em;
  font-weight: 600;
  border-bottom: 2px solid var(--cttir-topic);
}
tbody td, .table > tbody > tr > td, .table > tbody > tr > th,
.table > tfoot > tr > td {
  border-top: 1px solid var(--cttir-border);
}
.table-striped > tbody > tr:nth-of-type(odd),
.table-hover > tbody > tr:hover {
  background-color: var(--cttir-code-surface);
}

/* --- gt tables carry inline colours; restyle them to the page --- */
.gt_table {
  background-color: transparent !important;
  color: var(--cttir-text) !important;
  border-top: 2px solid var(--cttir-topic) !important;
}
.gt_heading, .gt_title, .gt_subtitle, .gt_col_headings, .gt_col_heading,
.gt_column_spanner_outer, .gt_row, .gt_stub, .gt_sourcenotes, .gt_footnotes {
  background-color: transparent !important;
  color: var(--cttir-text) !important;
  border-color: var(--cttir-border) !important;
}
.gt_col_heading {
  color: var(--cttir-heading) !important;
  font-weight: 600 !important;
}
.gt_col_headings {
  border-bottom: 2px solid var(--cttir-topic) !important;
}
.gt_group_heading, .gt_empty_group_heading, .gt_striped {
  background-color: var(--cttir-code-surface) !important;
  color: var(--cttir-heading) !important;
}

/* --- Panels, wells and tabs, including the workflowr report --- */
.panel, .well, .tab-content {
  background-color: var(--cttir-page);
  border-color: var(--cttir-border);
  color: var(--cttir-text);
}
.panel {
  border-radius: 4px;
  box-shadow: none;
}
.panel-default > .panel-heading, .panel-primary > .panel-heading {
  background-color: var(--cttir-code-surface);
  border-color: var(--cttir-border);
  color: var(--cttir-heading);
}
.panel-primary {
  border-color: var(--cttir-border);
  border-left: 3px solid var(--cttir-topic);
}
.well {
  background-color: var(--cttir-code-surface);
  box-shadow: none;
}
.nav-tabs {
  border-bottom-color: var(--cttir-border);
}
.nav-tabs > li > a:hover {
  background-color: var(--cttir-code-surface);
  border-color: var(--cttir-border);
}
.nav-tabs > li.active > a,
.nav-tabs > li.active > a:hover,
.nav-tabs > li.active > a:focus {
  background-color: var(--cttir-page);
  border-color: var(--cttir-border) var(--cttir-border) transparent;
  color: var(--cttir-heading);
  box-shadow: inset 0 3px 0 var(--cttir-topic);
}
.nav-pills > li.active > a,
.nav-pills > li.active > a:hover,
.nav-pills > li.active > a:focus {
  background-color: var(--cttir-code-surface);
  color: var(--cttir-heading);
  box-shadow: inset 3px 0 0 var(--cttir-topic);
}

/* --- Callouts: neutral surface, accent or status colour on the left --- */
blockquote {
  border-left: 3px solid var(--cttir-topic);
  padding: 0.25rem 0 0.25rem 1rem;
  margin: 1em 0;
  font-size: inherit;
  color: var(--cttir-text);
}
blockquote p:last-child {
  margin-bottom: 0;
}
.alert {
  background-color: var(--cttir-code-surface);
  border: 1px solid var(--cttir-border);
  border-left: 3px solid var(--cttir-topic);
  color: var(--cttir-text);
}
.alert a, .alert .alert-link {
  color: var(--cttir-link);
}
.alert-success { border-left-color: <<s$green>>; }
.alert-warning { border-left-color: <<s$orange>>; }
.alert-danger { border-left-color: <<s$red>>; }

/* --- Badges and labels --- */
.badge, .label {
  background-color: var(--cttir-code-surface);
  border: 1px solid var(--cttir-border);
  color: var(--cttir-heading);
  font-weight: 600;
}

/* --- Footer --- */
footer, .footer, #footer {
  border-top: 1px solid var(--cttir-border);
  color: var(--cttir-muted);
}

/* --- Print: light tokens (set above), no navigation --- */
@media print {
  .navbar { display: none !important; }
  a { text-decoration: underline; }
  pre { page-break-inside: avoid; }
  h1, h2, h3 { page-break-after: avoid; }
  table { page-break-inside: avoid; }
}
', .open = "<<", .close = ">>")

  paste0(
    "/* ============================================================\n",
    "   reflowR theme: ", s$label, " (CTTIR v5 family ", s$family, ")\n",
    "   Generated from the CTTIR v5 design tokens -- do not edit by hand.\n",
    "   No fonts are downloaded: Inter and JetBrains Mono are used when\n",
    "   installed, otherwise the system fonts in the stacks below.\n",
    "   ============================================================ */\n\n",
    css_token_blocks(s), "\n\n", rules, "\n"
  )
}


#' Generate Theme SCSS from a Color Scheme
#'
#' Creates a Quarto-compatible SCSS file on the CTTIR v5 surface. Bootstrap
#' defaults take the light tokens (links use the link token, as in themakR);
#' the rules section adds the same `--cttir-*` custom properties as the site
#' CSS, a dark mode and the accent identity rules.
#'
#' @param scheme A scheme list from [reflow_scheme()].
#' @param output_path Character. File path for the output SCSS file.
#'
#' @return Invisibly returns the output path.
#'
#' @keywords internal
generate_scss <- function(scheme, output_path) {
  writeLines(site_scss(scheme), output_path)
  invisible(output_path)
}

site_scss <- function(scheme) {
  s <- scheme
  defaults <- glue::glue('
/*-- scss:defaults --*/

// CTTIR v5 tokens for the <<s$label>> family (<<s$family>>).
$cttir-topic: <<s$accent>>;
$cttir-page: <<s$page_light>>;
$cttir-text: <<s$text_light>>;
$cttir-heading: <<s$heading_light>>;
$cttir-link: <<s$link_light>>;
$cttir-code-surface: <<s$code_surface_light>>;
$cttir-border: <<s$border_light>>;
$cttir-muted: <<s$muted_light>>;

$primary: $cttir-link;
$body-bg: $cttir-page;
$body-color: $cttir-text;
$headings-color: $cttir-heading;
$link-color: $cttir-link;
$border-color: $cttir-border;
$code-bg: $cttir-code-surface;
$code-color: $cttir-text;
$font-family-sans-serif: "<<s$font_body>>", system-ui, sans-serif;
$font-family-monospace: "<<s$font_code>>", ui-monospace, monospace;
$headings-font-weight: 600;

$navbar-bg: $cttir-page;
$navbar-fg: $cttir-heading;
$navbar-hl: $cttir-heading;

$toc-active-border: $cttir-topic;
$toc-inactive-border: $cttir-border;
', .open = "<<", .close = ">>")

  rules <- glue::glue('
/*-- scss:rules --*/

body {
  background-color: var(--cttir-page);
  color: var(--cttir-text);
  font-family: var(--cttir-font-body);
}
h1, h2, h3, h4, h5, h6 {
  color: var(--cttir-heading);
}
h1, h1.title {
  border-bottom: 3px solid var(--cttir-topic);
  padding-bottom: 0.3em;
}
h2 {
  border-bottom: 1px solid var(--cttir-border);
  padding-bottom: 0.25em;
}
a, a:visited, a:hover {
  color: var(--cttir-link);
}
.navbar {
  background-color: var(--cttir-page);
  box-shadow: inset 0 -3px 0 var(--cttir-topic);
}
.navbar-brand, .navbar-title {
  color: var(--cttir-heading);
  font-family: var(--cttir-font-brand);
  font-weight: var(--cttir-brand-weight);
}
.navbar .nav-link, .navbar .nav-link:hover {
  color: var(--cttir-heading);
}
#TOC .nav-link.active, nav[role="doc-toc"] a.active {
  color: var(--cttir-heading);
  border-left-color: var(--cttir-topic);
}
body pre, body div.sourceCode {
  background-color: var(--cttir-code-surface);
  border: 1px solid var(--cttir-border);
  color: var(--cttir-text);
}
:not(pre) > code {
  background-color: var(--cttir-code-surface);
  color: var(--cttir-text);
}
<<css_syntax_rules()>>
table thead th {
  color: var(--cttir-heading);
  font-family: var(--cttir-font-code);
  font-weight: 600;
  border-bottom: 2px solid var(--cttir-topic);
}
blockquote, .callout {
  border-left: 3px solid var(--cttir-topic);
  color: var(--cttir-text);
}
.badge {
  background-color: var(--cttir-code-surface);
  color: var(--cttir-heading);
}
::selection {
  background-color: var(--cttir-face);
  color: var(--cttir-heading);
}
', .open = "<<", .close = ">>")

  paste0(defaults, "\n\n", rules, "\n\n", css_token_blocks(s), "\n")
}
