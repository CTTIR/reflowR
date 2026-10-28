#' Get Base64-Encoded SVG Icon for a Scheme
#'
#' Reads the CTTIR v5 family icon from the package's `inst/icons/` directory
#' and returns it as a base64-encoded data URI suitable for embedding in HTML.
#' The icon is the v5 hex on a light surface: a tinted face, the family accent
#' as rim and a small family glyph, with a transparent exterior.
#'
#' @param scheme_name Character. A v5 family key (e.g., `"clinical"`) or one
#'   of the earlier scheme names, which resolve to their v5 family icon.
#'
#' @return A character string containing the base64 data URI
#'   (e.g., `"data:image/svg+xml;base64,..."`).
#'
#' @examples
#' uri <- reflow_icon_base64("clinical")
#' nchar(uri)
#'
#' @export
reflow_icon_base64 <- function(scheme_name) {
  if (is.character(scheme_name) && length(scheme_name) == 1L &&
      scheme_name %in% names(.scheme_aliases)) {
    scheme_name <- scheme_family(scheme_name)
  }
  icon_path <- system.file("icons", paste0("icon_", scheme_name, ".svg"),
                           package = "reflowR")
  if (icon_path == "") {
    cli::cli_abort("Icon not found for scheme {.val {scheme_name}}")
  }
  svg_text <- paste(readLines(icon_path, warn = FALSE), collapse = "\n")
  paste0("data:image/svg+xml;base64,",
         base64enc::base64encode(charToRaw(svg_text)))
}
