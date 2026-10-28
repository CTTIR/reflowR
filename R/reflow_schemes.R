#' List Available Colour Schemes
#'
#' Prints the CTTIR v5 topic families with their labels and accents, followed
#' by the earlier scheme names that remain accepted as aliases, and returns
#' the family keys invisibly.
#'
#' @return Invisibly returns a character vector of the nine v5 family keys.
#'
#' @examples
#' reflow_schemes()
#'
#' @export
reflow_schemes <- function() {
  schemes <- names(.cttir_schemes())
  info <- vapply(schemes, function(s) {
    sc <- .cttir_schemes()[[s]]
    paste0(sc$label, "  (", sc$accent, ")")
  }, character(1))

  cli::cli_h2("Available reflowR Schemes (CTTIR v5 families)")
  for (i in seq_along(schemes)) {
    cli::cli_text("{.val {schemes[i]}} -- {info[i]}")
  }
  aliases <- paste0(names(.scheme_aliases), " -> ", .scheme_aliases)
  cli::cli_text("Earlier names remain aliases: {.val {aliases}}.")
  invisible(schemes)
}
