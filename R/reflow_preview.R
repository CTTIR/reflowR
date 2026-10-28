#' Preview a Color Scheme
#'
#' Opens an HTML preview in the RStudio viewer (or browser) showing the
#' family icon on a flat navbar with its accent rule, the light and dark
#' surfaces, the identity colours and the Okabe-Ito palette.
#'
#' @param scheme Character. A v5 family key (e.g., `"clinical"`) or an earlier
#'   scheme name, see [reflow_scheme()].
#'
#' @return Invisibly returns the scheme list.
#'
#' @examples
#' if (interactive()) {
#'   reflow_preview("clinical")
#'   reflow_preview("brand")
#' }
#'
#' @export
reflow_preview <- function(scheme = "brand") {
  s <- reflow_scheme(scheme)
  ink <- function(colour) {
    if (.cttir_contrast(colour, s$heading_light) >= .cttir_contrast(colour, s$page_light)) {
      s$heading_light
    } else {
      s$page_light
    }
  }
  swatch <- function(label, colour) {
    glue::glue(
      '<div style="display:inline-block;width:104px;height:60px;',
      'background-color:{colour};color:{ink(colour)};text-align:center;',
      'line-height:1.35;padding-top:12px;box-sizing:border-box;margin:4px;',
      'border-radius:4px;font-size:11px;border:1px solid {s$border_light};">',
      '{label}<br><code>{colour}</code></div>'
    )
  }
  row <- function(fields, suffix = "") {
    paste(vapply(fields, function(f) swatch(f, s[[paste0(f, suffix)]]), character(1)),
          collapse = "")
  }
  surfaces <- c("page", "text", "heading", "link", "code_surface", "border", "muted")
  palette <- paste(vapply(seq_along(s$palette), function(i) {
    swatch(i, s$palette[i])
  }, character(1)), collapse = "")

  html <- glue::glue('
<html>
<head><title>reflowR Preview: {s$label}</title></head>
<body style="background:{s$page_light};color:{s$text_light};margin:0;
  font-family:\'{s$font_body}\',system-ui,sans-serif;">
<div style="display:flex;align-items:center;gap:10px;padding:10px 20px;
  background:{s$page_light};box-shadow:inset 0 -3px 0 {s$accent};">
  <img src="{reflow_icon_base64(s$family)}" alt="" height="30">
  <span style="font-family:\'{s$font_brand}\',ui-monospace,monospace;
    font-weight:{s$brand_weight};color:{s$heading_light};">{s$label}</span>
  <span style="color:{s$muted_light};">CTTIR v5 family <code>{s$family}</code></span>
</div>
<div style="max-width:780px;margin:24px auto;padding:0 20px;">
<h2 style="color:{s$heading_light};">Identity</h2>
<div>{swatch("accent", s$accent)}{swatch("face (light)", s$face_light)}{swatch("face (dark)", s$face_dark)}</div>
<h2 style="color:{s$heading_light};">Light surfaces</h2>
<div>{row(surfaces, "_light")}</div>
<h2 style="color:{s$heading_light};">Dark surfaces</h2>
<div>{row(surfaces, "_dark")}</div>
<h2 style="color:{s$heading_light};">Palette (Okabe-Ito, accent first)</h2>
<div>{palette}</div>
</div>
</body>
</html>')

  tmpf <- tempfile(fileext = ".html")
  writeLines(html, tmpf)
  viewer <- getOption("viewer", utils::browseURL)
  viewer(tmpf)

  invisible(s)
}
