.onAttach <- function(libname, pkgname) {
  brand <- cli::make_ansi_style(reflow_scheme("brand")$accent)
  packageStartupMessage(
    cli::style_bold(brand("reflowR")), " v", utils::packageVersion("reflowR"),
    " \U2014 Themed Workflowr Extensions (CTTIR v5)\n",
    "Use ", cli::style_bold("reflow_init()"), " to get started.\n",
    "See ", cli::style_bold("reflow_schemes()"), " and ",
    cli::style_bold("reflow_presets()"), " for options."
  )
}
