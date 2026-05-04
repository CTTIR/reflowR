.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    cli::col_cyan("reflowR"), " v", utils::packageVersion("reflowR"),
    " \U2014 Themed Workflowr Extensions\n",
    "Use ", cli::col_green("reflow_init()"), " to get started.\n",
    "See ", cli::col_blue("reflow_schemes()"), " and ",
    cli::col_blue("reflow_presets()"), " for options."
  )
}
