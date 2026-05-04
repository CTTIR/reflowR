#' Generate Theme CSS from a Color Scheme
#'
#' Creates a CSS file with custom properties and styled selectors based on the
#' provided color scheme.
#'
#' @param scheme A scheme list from [reflow_scheme()].
#' @param output_path Character. File path for the output CSS file.
#'
#' @return Invisibly returns the output path.
#'
#' @keywords internal
generate_css <- function(scheme, output_path) {
  s <- scheme
  css <- glue::glue('
/* ============================================================
   reflowR Theme: {s$label}
   Auto-generated -- do not edit manually
   ============================================================ */

/* --- CSS Custom Properties --- */
:root {{
  --primary: {s$primary};
  --primary-light: {s$primary_light};
  --accent: {s$accent};
  --secondary: {s$secondary};
  --light-grey: {s$light_grey};
  --grey: {s$grey};
  --dark: {s$dark};
  --white: {s$white};
  --gradient-from: {s$gradient_from};
  --gradient-to: {s$gradient_to};
  --font-family: "Source Sans Pro", -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
}}

/* --- Body --- */
body {{
  background-color: #FFFFFF !important;
  font-family: var(--font-family);
  color: #333333;
  line-height: 1.7;
}}

/* --- Navbar --- */
.navbar {{
  background: linear-gradient(135deg, var(--gradient-from), var(--gradient-to)) !important;
  border: none !important;
  box-shadow: 0 2px 10px rgba(0, 0, 0, 0.15);
  min-height: 56px;
}}
.navbar-default {{
  background: linear-gradient(135deg, var(--gradient-from), var(--gradient-to)) !important;
  border: none !important;
}}
.navbar-default .navbar-brand,
.navbar-default .navbar-nav > li > a {{
  color: var(--white) !important;
  font-weight: 600;
  transition: opacity 0.2s ease;
}}
.navbar-default .navbar-nav > li > a:hover,
.navbar-default .navbar-nav > li > a:focus {{
  color: var(--white) !important;
  opacity: 0.85;
  background-color: rgba(255, 255, 255, 0.1) !important;
}}
.navbar-default .navbar-nav > .active > a,
.navbar-default .navbar-nav > .active > a:hover,
.navbar-default .navbar-nav > .active > a:focus {{
  color: var(--white) !important;
  background-color: rgba(255, 255, 255, 0.15) !important;
}}
.navbar-brand img {{
  display: inline-block;
  margin-right: 8px;
  vertical-align: middle;
}}

/* --- Dropdown Menus --- */
.navbar-default .navbar-nav > .open > a,
.navbar-default .navbar-nav > .open > a:hover {{
  background-color: rgba(255, 255, 255, 0.15) !important;
  color: var(--white) !important;
}}
.dropdown-menu {{
  border: 1px solid var(--light-grey);
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
  border-radius: 4px;
}}
.dropdown-menu > li > a {{
  color: var(--dark) !important;
  padding: 8px 20px;
  transition: background-color 0.15s ease;
}}
.dropdown-menu > li > a:hover {{
  background-color: var(--primary) !important;
  color: var(--white) !important;
}}

/* --- Headings --- */
h1, h2, h3, h4, h5, h6 {{
  color: var(--dark);
  font-weight: 700;
}}
h1 {{ border-bottom: 3px solid var(--primary); padding-bottom: 0.4em; }}
h2 {{ border-bottom: 2px solid var(--primary-light); padding-bottom: 0.3em; }}
h3 {{ color: var(--accent); }}

/* --- Links --- */
a {{ color: var(--primary); text-decoration: none; transition: color 0.2s ease; }}
a:hover {{ color: var(--accent); text-decoration: underline; }}

/* --- TOC Sidebar --- */
.tocify {{
  border: 1px solid var(--light-grey);
  border-radius: 6px;
  box-shadow: 0 1px 4px rgba(0, 0, 0, 0.05);
}}
.tocify .tocify-header {{
  font-weight: 600;
}}
.tocify-item.active {{
  background-color: var(--primary) !important;
  color: var(--white) !important;
  border-radius: 4px;
}}
.tocify-item.active a {{
  color: var(--white) !important;
}}
.tocify ul, .tocify li {{
  line-height: 1.6;
}}

/* --- Tables --- */
table {{
  width: 100%;
  border-collapse: collapse;
  margin-bottom: 1.5rem;
}}
thead th {{
  background-color: var(--primary) !important;
  color: var(--white) !important;
  font-weight: 600;
  padding: 10px 12px;
  border-bottom: 2px solid var(--accent);
}}
tbody td {{
  padding: 8px 12px;
  border-bottom: 1px solid var(--light-grey);
}}
tbody tr:hover {{
  background-color: {s$light_grey} !important;
}}
tbody tr:nth-child(even) {{
  background-color: {s$light_grey}80;
}}

/* --- gt tables --- */
.gt_table {{
  border-top: 3px solid var(--primary) !important;
}}
.gt_col_heading {{
  background-color: var(--primary) !important;
  color: var(--white) !important;
  font-weight: 600 !important;
}}
.gt_group_heading {{
  background-color: var(--primary-light) !important;
  color: var(--white) !important;
}}
.gt_row:hover {{
  background-color: {s$light_grey} !important;
}}
.gt_striped {{
  background-color: {s$light_grey}80 !important;
}}

/* --- Code Blocks --- */
pre {{
  background-color: #F8F9FA;
  border: 1px solid #E9ECEF;
  border-left: 4px solid var(--primary);
  border-radius: 4px;
  padding: 12px 16px;
  font-size: 0.9em;
}}
code {{
  color: var(--accent);
  background-color: #F8F9FA;
  padding: 2px 6px;
  border-radius: 3px;
  font-size: 0.9em;
}}
pre code {{
  color: inherit;
  background-color: transparent;
  padding: 0;
}}

/* --- Buttons --- */
.btn-primary {{
  background-color: var(--primary) !important;
  border-color: var(--accent) !important;
  color: var(--white) !important;
  font-weight: 600;
  transition: all 0.2s ease;
}}
.btn-primary:hover {{
  background-color: var(--accent) !important;
  border-color: var(--secondary) !important;
  transform: translateY(-1px);
  box-shadow: 0 2px 6px rgba(0, 0, 0, 0.15);
}}

/* --- Panels --- */
.panel-primary {{
  border-color: var(--primary);
}}
.panel-primary > .panel-heading {{
  background-color: var(--primary) !important;
  border-color: var(--accent) !important;
  color: var(--white) !important;
}}
.panel {{
  border-radius: 6px;
  box-shadow: 0 1px 4px rgba(0, 0, 0, 0.08);
}}

/* --- Alerts --- */
.alert-info {{
  background-color: {s$light_grey};
  border-color: var(--primary-light);
  color: var(--dark);
  border-left: 4px solid var(--primary);
}}
.alert-success {{
  border-left: 4px solid #28a745;
}}
.alert-warning {{
  border-left: 4px solid #ffc107;
}}
.alert-danger {{
  border-left: 4px solid #dc3545;
}}

/* --- Badges --- */
.badge {{
  background-color: var(--primary) !important;
  color: var(--white) !important;
  font-weight: 600;
  padding: 4px 8px;
  border-radius: 12px;
}}

/* --- Blockquotes --- */
blockquote {{
  border-left: 4px solid var(--primary);
  padding: 10px 20px;
  margin: 1em 0;
  background-color: {s$light_grey};
  border-radius: 0 4px 4px 0;
  color: var(--dark);
}}
blockquote p:last-child {{
  margin-bottom: 0;
}}

/* --- Selection Highlight --- */
::selection {{
  background-color: {s$primary_light};
  color: var(--white);
}}
::-moz-selection {{
  background-color: {s$primary_light};
  color: var(--white);
}}

/* --- Scrollbar --- */
::-webkit-scrollbar {{
  width: 8px;
}}
::-webkit-scrollbar-track {{
  background: var(--light-grey);
}}
::-webkit-scrollbar-thumb {{
  background: var(--grey);
  border-radius: 4px;
}}
::-webkit-scrollbar-thumb:hover {{
  background: var(--primary);
}}

/* --- Code Folding --- */
.btn-default.btn-xs {{
  color: var(--primary);
  border-color: var(--primary-light);
}}
.btn-default.btn-xs:hover {{
  background-color: var(--primary);
  color: var(--white);
}}

/* --- workflowr status bar --- */
.wflow-status {{
  margin-bottom: 1.5rem;
  padding: 10px 15px;
  background-color: {s$light_grey};
  border-radius: 4px;
  border-left: 4px solid var(--primary);
}}

/* --- Figure captions --- */
.figure .caption, figcaption {{
  color: var(--grey);
  font-style: italic;
  margin-top: 0.5em;
  text-align: center;
}}

/* --- Print Styles --- */
@media print {{
  .navbar {{ display: none !important; }}
  body {{ background-color: #FFFFFF !important; color: #000000 !important; }}
  a {{ color: #000000 !important; text-decoration: underline; }}
  pre {{ border: 1px solid #cccccc; page-break-inside: avoid; }}
  h1, h2, h3 {{ page-break-after: avoid; }}
  table {{ page-break-inside: avoid; }}
  thead th {{ background-color: #EEEEEE !important; color: #000000 !important; }}
}}
')
  writeLines(css, output_path)
  invisible(output_path)
}


#' Generate Theme SCSS from a Color Scheme
#'
#' Creates a Quarto-compatible SCSS file with custom properties.
#'
#' @param scheme A scheme list from [reflow_scheme()].
#' @param output_path Character. File path for the output SCSS file.
#'
#' @return Invisibly returns the output path.
#'
#' @keywords internal
generate_scss <- function(scheme, output_path) {
  s <- scheme
  scss <- glue::glue('
/*-- scss:defaults --*/

$primary: {s$primary};
$primary-light: {s$primary_light};
$accent: {s$accent};
$secondary: {s$secondary};
$light-grey: {s$light_grey};
$grey: {s$grey};
$dark: {s$dark};

$body-bg: #FFFFFF;
$body-color: #333333;
$link-color: $primary;
$font-family-sans-serif: "Source Sans Pro", -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;

$navbar-bg: $primary;
$navbar-fg: #FFFFFF;

$toc-active-border: $primary;
$toc-inactive-border: $light-grey;

/*-- scss:rules --*/

body {{
  background-color: #FFFFFF !important;
  line-height: 1.7;
}}

h1 {{ border-bottom: 3px solid $primary; padding-bottom: 0.4em; color: $dark; }}
h2 {{ border-bottom: 2px solid $primary-light; padding-bottom: 0.3em; color: $dark; }}
h3 {{ color: $accent; }}

pre {{
  border-left: 4px solid $primary;
  background-color: #F8F9FA;
}}

code {{
  color: $accent;
}}

blockquote {{
  border-left: 4px solid $primary;
  background-color: $light-grey;
  padding: 10px 20px;
  border-radius: 0 4px 4px 0;
}}

table thead th {{
  background-color: $primary !important;
  color: #FFFFFF !important;
}}

.badge {{
  background-color: $primary;
}}

::selection {{
  background-color: $primary-light;
  color: #FFFFFF;
}}
')
  writeLines(scss, output_path)
  invisible(output_path)
}
