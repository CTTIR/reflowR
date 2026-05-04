#' Preview a Color Scheme
#'
#' Opens an HTML preview in the RStudio viewer (or browser) showing
#' color swatches, the scheme icon, and the navbar gradient.
#'
#' @param scheme Character. Scheme name (e.g., `"clinical"`).
#'
#' @return Invisibly returns the scheme list.
#'
#' @examples
#' \dontrun{
#' reflow_preview("clinical")
#' reflow_preview("basic")
#' }
#'
#' @export
reflow_preview <- function(scheme = "basic") {
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg htmltools} is required for preview.")
  }

  s <- reflow_scheme(scheme)

  # Build color swatches
  color_names <- c("primary", "primary_light", "accent", "secondary",
                   "light_grey", "grey", "dark", "white")
  swatches <- vapply(color_names, function(cn) {
    color <- s[[cn]]
    if (is.null(color)) return("")
    text_color <- if (cn %in% c("light_grey", "white")) "#333" else "#FFF"
    glue::glue(
      '<div style="display:inline-block;width:100px;height:60px;',
      'background-color:{color};color:{text_color};text-align:center;',
      'line-height:60px;margin:4px;border-radius:6px;font-size:11px;',
      'border:1px solid #ddd;">{cn}<br>{color}</div>'
    )
  }, character(1))

  palette_swatches <- vapply(seq_along(s$palette), function(i) {
    color <- s$palette[i]
    glue::glue(
      '<div style="display:inline-block;width:80px;height:50px;',
      'background-color:{color};color:#FFF;text-align:center;',
      'line-height:50px;margin:4px;border-radius:6px;font-size:10px;',
      'border:1px solid #ddd;">{i}<br>{color}</div>'
    )
  }, character(1))

  html <- glue::glue('
<html>
<head><title>reflowR Preview: {s$label}</title></head>
<body style="font-family:sans-serif;max-width:700px;margin:40px auto;padding:20px;">
<div style="background:linear-gradient(135deg,{s$gradient_from},{s$gradient_to});
  padding:20px;border-radius:8px;color:white;margin-bottom:20px;">
  <h1 style="margin:0;">{s$navbar_icon} {s$label}</h1>
  <p style="margin:5px 0 0 0;opacity:0.9;">reflowR scheme preview</p>
</div>
<h2>Theme Colors</h2>
<div>{paste(swatches, collapse = "")}</div>
<h2>Palette (8 colors)</h2>
<div>{paste(palette_swatches, collapse = "")}</div>
<h2>Gradient</h2>
<div style="height:40px;border-radius:6px;
  background:linear-gradient(135deg,{s$gradient_from},{s$gradient_to});"></div>
</body>
</html>')

  tmpf <- tempfile(fileext = ".html")
  writeLines(html, tmpf)
  viewer <- getOption("viewer", utils::browseURL)
  viewer(tmpf)

  invisible(s)
}
