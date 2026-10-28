#' Generate Themed _site.yml
#'
#' Creates a `_site.yml` file in the project's `analysis/` directory with
#' the family icon and project name in the navbar, pipeline menu entries and
#' the themed `workflowr::wflow_html` output. The site is written to `docs/`.
#'
#' @param project_dir Character. Path to the project root.
#' @param name Character. Project name.
#' @param scheme A scheme list from [reflow_scheme()].
#' @param preset A preset list from [reflow_preset()].
#' @param github_url Character or NULL. GitHub URL for navbar link.
#' @param website Character or NULL. Website URL for navbar link.
#'
#' @return Invisibly returns the output file path.
#'
#' @keywords internal
generate_site_yml <- function(project_dir, name, scheme, preset,
                              github_url = NULL, website = NULL) {
  p <- preset
  scalar <- strict_yaml_scalar
  html <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }

  # Build pipeline menu items
  menu_items <- unlist(lapply(p$files, function(f) {
    step_num <- sub("^(\\d+)_.*", "\\1", f)
    step_name <- gsub("_", " ", sub("^\\d+_", "", f))
    step_name <- paste0(toupper(substring(step_name, 1, 1)),
                        substring(step_name, 2))
    c(paste0("    - text: ", scalar(paste(step_num, "--", step_name))),
      paste0("      href: ", f, ".html"))
  }), use.names = FALSE)

  # Build right nav
  right_items <- character(0)
  if (!is.null(github_url)) {
    right_items <- c(right_items, "  - icon: fa-github",
                     paste0("    href: ", scalar(github_url)))
  }
  if (!is.null(website)) {
    right_items <- c(right_items, "  - icon: fa-globe",
                     paste0("    href: ", scalar(website)))
  }
  if (length(right_items) > 0) right_items <- c("  right:", right_items)

  # The family icon is copied to docs/assets/icon.svg next to the pages.
  title <- paste0('<img src="assets/icon.svg" alt="" height="30"> ', html(name))

  site_yml <- c(
    paste0("name: ", scalar(name)),
    "output_dir: ../docs",
    "navbar:",
    paste0("  title: ", scalar(title)),
    "  type: default",
    "  left:",
    '  - text: "Home"',
    "    href: index.html",
    '  - text: "Pipeline"',
    "    icon: fa-flask",
    "    menu:",
    menu_items,
    '  - text: "About"',
    "    href: about.html",
    '  - text: "License"',
    "    href: license.html",
    right_items,
    "output:",
    "  workflowr::wflow_html:",
    "    toc: true",
    "    toc_float:",
    "      collapsed: false",
    "    toc_depth: 3",
    "    number_sections: true",
    "    theme: flatly",
    "    highlight: tango",
    "    code_folding: show",
    "    css: theme.css",
    "    fig_width: 8",
    "    fig_height: 5",
    "    fig_retina: 2"
  )

  writeLines(site_yml, fs::path(project_dir, "analysis", "_site.yml"))
  invisible(fs::path(project_dir, "analysis", "_site.yml"))
}
