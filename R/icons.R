#' Get Base64-Encoded SVG Icon for a Scheme
#'
#' Reads the scheme-specific SVG icon from the package's `inst/icons/`
#' directory and returns it as a base64-encoded data URI suitable for
#' embedding in HTML.
#'
#' @param scheme_name Character. The scheme name (e.g., `"clinical"`).
#'
#' @return A character string containing the base64 data URI
#'   (e.g., `"data:image/svg+xml;base64,..."`).
#'
#' @examples
#' \donttest{
#' uri <- reflow_icon_base64("clinical")
#' nchar(uri)
#' }
#'
#' @export
reflow_icon_base64 <- function(scheme_name) {
  icon_path <- system.file("icons", paste0("icon_", scheme_name, ".svg"),
                           package = "reflowR")
  if (icon_path == "") {
    cli::cli_abort("Icon not found for scheme {.val {scheme_name}}")
  }
  svg_text <- paste(readLines(icon_path, warn = FALSE), collapse = "\n")
  paste0("data:image/svg+xml;base64,",
         base64enc::base64encode(charToRaw(svg_text)))
}
