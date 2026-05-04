#' List Available Depth Presets
#'
#' Prints a formatted table of all available reflowR depth presets and
#' returns their names invisibly.
#'
#' @return Invisibly returns a character vector of preset names.
#'
#' @examples
#' reflow_presets()
#'
#' @export
reflow_presets <- function() {
  presets <- names(.presets)
  info <- vapply(presets, function(p) {
    pr <- .presets[[p]]
    paste0(pr$label, "  (", length(pr$files), " files)")
  }, character(1))

  cli::cli_h2("Available reflowR Presets")
  for (i in seq_along(presets)) {
    cli::cli_text("{.val {presets[i]}} -- {info[i]}")
  }
  invisible(presets)
}
