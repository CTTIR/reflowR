#' Get a Colour Scheme Definition
#'
#' Returns the site scheme for one CTTIR v5 topic family. Every colour, surface
#' and font is derived from `inst/cttir/design_tokens.csv`, the token table
#' shared with hexmakR and the themakR documentation template.
#'
#' @param name Character. One of the v5 families `"spectral"`, `"spatial"`,
#'   `"omics"`, `"clinical"`, `"methods"`, `"simulation"`, `"evidence"`,
#'   `"platform"` or `"brand"`. The earlier names remain accepted as aliases:
#'   `"basic"` (brand), `"code"` (methods), `"special"` (evidence) and
#'   `"other"` (platform). An alias prints a one-time message per session.
#'
#' @return A named list. `family`, `label`, `accent` and `navbar_icon`
#'   identify the family. Surfaces are given per colour mode as `page_*`,
#'   `text_*`, `heading_*`, `link_*`, `code_surface_*`, `border_*` and
#'   `muted_*` with suffix `_light` or `_dark`; `face_light` and `face_dark`
#'   are the tinted tile faces, `syntax_light` and `syntax_dark` the code token
#'   colours, `palette` the Okabe-Ito topic colours starting with the family
#'   accent, and `font_body`, `font_code`, `font_brand` and `brand_weight` the
#'   typography. The earlier fields (`primary`, `primary_light`, `secondary`,
#'   `light_grey`, `grey`, `dark`, `white`, `gradient_from`, `gradient_to` and
#'   the named hues) remain available and are derived from the same tokens.
#'
#' @examples
#' s <- reflow_scheme("clinical")
#' s$accent
#' s$palette
#'
#' @seealso [reflow_schemes()] for listing all available schemes.
#' @export
reflow_scheme <- function(name) {
  .cttir_schemes()[[scheme_family(name)]]
}

# Installed tokens are read once per session and shared by all generators.
.cttir_cache <- new.env(parent = emptyenv())

# Earlier scheme names map onto the v5 family that replaced them.
.scheme_aliases <- c(basic = "brand", code = "methods", special = "evidence",
                     other = "platform")

# Labels follow cttir_family_labels in tools/branding/R/palette.R.
.cttir_family_labels <- c(
  spectral   = "Spectral imaging",
  spatial    = "Spatial biology",
  omics      = "Molecular data",
  clinical   = "Clinical science",
  methods    = "Learning & inference",
  simulation = "Systems & simulation",
  evidence   = "Evidence & discovery",
  platform   = "Research infrastructure",
  brand      = "Science, connected"
)

# Categorical order: strong hues first, yellow (weakest on light pages) last.
.cttir_palette_order <- c("spectral", "clinical", "spatial", "simulation",
                          "omics", "methods", "platform", "evidence")

# Code token colours reuse topic hues: v5 edge ink on light surfaces, the mid
# tint on dark ones. Comments use the muted text colour.
.cttir_syntax_families <- c(keyword = "methods", string = "spatial",
                            number = "simulation", "function" = "omics",
                            alert = "clinical")

.cttir_tokens <- function() {
  if (is.null(.cttir_cache$tokens)) {
    path <- system.file("cttir", "design_tokens.csv", package = "reflowR")
    if (!nzchar(path)) cli::cli_abort("The installed CTTIR design tokens are missing.")
    x <- utils::read.csv(path, stringsAsFactors = FALSE)
    tokens <- x$value
    names(tokens) <- paste(x$group, x$token, sep = ".")
    .cttir_cache$tokens <- tokens
  }
  .cttir_cache$tokens
}

.cttir_token <- function(group, token) {
  key <- paste(group, token, sep = ".")
  tokens <- .cttir_tokens()
  if (!key %in% names(tokens)) cli::cli_abort("Missing design token {.val {key}}.")
  tokens[[key]]
}

.cttir_topics <- function() {
  tokens <- .cttir_tokens()
  topics <- tokens[startsWith(names(tokens), "topic.")]
  names(topics) <- sub("^topic[.]", "", names(topics))
  topics
}

# Same arithmetic as cttir_mix(): weight t on a, 1 - t on b.
.cttir_mix <- function(a, b, t) {
  z <- grDevices::col2rgb(a) * t + grDevices::col2rgb(b) * (1 - t)
  grDevices::rgb(z[1], z[2], z[3], maxColorValue = 255)
}

# Platform keeps its black accent as the rim; derived tints use the material.
.cttir_material <- function(family) {
  if (identical(family, "platform")) "#8FAABD" else .cttir_topics()[[family]]
}

# WCAG 2 relative luminance and contrast ratio.
.cttir_luminance <- function(colour) {
  v <- grDevices::col2rgb(colour) / 255
  v <- ifelse(v <= 0.04045, v / 12.92, ((v + 0.055) / 1.055)^2.4)
  sum(c(0.2126, 0.7152, 0.0722) * v)
}

.cttir_contrast <- function(a, b) {
  l <- sort(c(.cttir_luminance(a), .cttir_luminance(b)), decreasing = TRUE)
  (l[1] + 0.05) / (l[2] + 0.05)
}

scheme_family <- function(name) {
  name <- match.arg(name, c(names(.cttir_family_labels), names(.scheme_aliases)))
  if (!name %in% names(.scheme_aliases)) return(name)
  family <- .scheme_aliases[[name]]
  if (!name %in% .cttir_cache$aliases_noted) {
    label <- .cttir_family_labels[[family]]
    cli::cli_inform(c("i" = paste(
      "Scheme {.val {name}} is now the CTTIR v5 family {.val {family}}",
      "({label}). This message is shown once per session.")))
    .cttir_cache$aliases_noted <- c(.cttir_cache$aliases_noted, name)
  }
  family
}

.cttir_schemes <- function() {
  if (is.null(.cttir_cache$schemes)) {
    families <- names(.cttir_topics())
    .cttir_cache$schemes <- stats::setNames(lapply(families, .cttir_scheme), families)
  }
  .cttir_cache$schemes
}

.cttir_scheme <- function(family) {
  topics <- .cttir_topics()
  accent <- topics[[family]]
  material <- .cttir_material(family)
  mode <- function(group) {
    fields <- c("page", "text", "heading", "link", "code_surface", "border")
    values <- vapply(fields, function(f) .cttir_token(group, f), character(1))
    # Secondary text as in themakR: 75 percent text over the page.
    c(values, muted = .cttir_mix(values[["text"]], values[["page"]], 0.75))
  }
  light <- mode("light")
  dark <- mode("dark")
  syntax <- function(tint, muted) {
    hues <- vapply(.cttir_syntax_families, function(f) tint(.cttir_material(f)),
                   character(1))
    c(hues, comment = muted)
  }
  primary_light <- .cttir_mix(material, "#FFFFFF", 0.30)
  secondary <- .cttir_mix(material, "#142235", 0.36)
  palette <- unique(c(accent, unname(topics[.cttir_palette_order])))

  list(
    name          = family,
    label         = .cttir_family_labels[[family]],
    navbar_icon   = paste0("icon_", family, ".svg"),
    primary       = accent,
    primary_light = primary_light,
    accent        = accent,
    secondary     = secondary,
    yellow        = topics[["evidence"]],
    orange        = topics[["simulation"]],
    red           = topics[["clinical"]],
    green         = topics[["spatial"]],
    blue          = topics[["spectral"]],
    light_grey    = light[["code_surface"]],
    grey          = light[["muted"]],
    dark          = light[["heading"]],
    white         = light[["page"]],
    gradient_from = primary_light,
    gradient_to   = secondary,
    palette       = palette,
    family        = family,
    material      = material,
    face_light    = .cttir_mix(accent, light[["page"]], 0.14),
    face_dark     = .cttir_mix(accent, dark[["page"]], 0.24),
    page_light         = light[["page"]],
    text_light         = light[["text"]],
    heading_light      = light[["heading"]],
    link_light         = light[["link"]],
    code_surface_light = light[["code_surface"]],
    border_light       = light[["border"]],
    muted_light        = light[["muted"]],
    page_dark          = dark[["page"]],
    text_dark          = dark[["text"]],
    heading_dark       = dark[["heading"]],
    link_dark          = dark[["link"]],
    code_surface_dark  = dark[["code_surface"]],
    border_dark        = dark[["border"]],
    muted_dark         = dark[["muted"]],
    syntax_light  = syntax(function(m) .cttir_mix(m, "#14283A", 0.5), light[["muted"]]),
    syntax_dark   = syntax(function(m) .cttir_mix(m, "#FFFFFF", 0.78), dark[["muted"]]),
    font_body     = .cttir_token("type", "body_font"),
    font_code     = .cttir_token("type", "code_font"),
    font_brand    = .cttir_token("type", "brand_font"),
    brand_weight  = as.integer(.cttir_token("type", "brand_weight"))
  )
}
