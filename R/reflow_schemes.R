#' List Available Color Schemes
#'
#' Prints a formatted table of all available reflowR color schemes and
#' returns their names invisibly.
#'
#' @return Invisibly returns a character vector of scheme names.
#'
#' @examples
#' reflow_schemes()
#'
#' @export
reflow_schemes <- function() {
  schemes <- names(.schemes)
  info <- vapply(schemes, function(s) {
    sc <- .schemes[[s]]
    paste0(sc$navbar_icon, "  ", sc$label, "  (", sc$primary, ")")
  }, character(1))

  cli::cli_h2("Available reflowR Schemes")
  for (i in seq_along(schemes)) {
    cli::cli_text("{.val {schemes[i]}} -- {info[i]}")
  }
  invisible(schemes)
}
