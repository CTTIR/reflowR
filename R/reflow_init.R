#' Initialize a Themed Workflowr Project
#'
#' Wraps \code{\link[workflowr]{wflow_start}} to create a standard workflowr
#' project, then extends it with a themed color scheme, structured Rmd analysis
#' pipeline, utility scripts, and a Quarto report template.
#'
#' @param directory Character. Path where the project will be created.
#'   Passed directly to \code{workflowr::wflow_start()}.
#' @param name Character. Project name (used in titles and navbar).
#'   If \code{NULL}, derived from the directory basename.
#' @param author Character. Full name of the primary author.
#' @param email Character. Author's email address (optional).
#' @param affiliation Character. Author's institutional affiliation (optional).
#' @param orcid Character. Author's ORCID iD (optional, format: "0000-0000-0000-0000").
#' @param website Character. URL to author's or project website (optional).
#' @param scheme Character. Color scheme to apply. One of \code{"clinical"},
#'   \code{"basic"}, \code{"code"}, \code{"special"}, \code{"other"}.
#'   Defaults to \code{"basic"}.
#' @param depth Character. Rmd pipeline depth. One of \code{"minimal"},
#'   \code{"standard"}, \code{"extended"}. Defaults to \code{"standard"}.
#' @param seed Integer. Random seed for workflowr reproducibility.
#'   Defaults to \code{as.integer(format(Sys.Date(), "\%Y\%m\%d"))}.
#' @param github_url Character. GitHub repository URL (optional).
#'   Adds a GitHub link to the navbar.
#' @param existing Logical. Is the \code{directory} an existing project?
#'   Passed to \code{workflowr::wflow_start(existing = ...)}. Default \code{FALSE}.
#' @param git Logical. Initialize git repository and make first commit?
#'   Default \code{TRUE}.
#' @param change_wd Logical. Change working directory to the new project?
#'   Default \code{TRUE} in interactive sessions.
#' @param user.name Character. Git user name (optional).
#'   Passed to \code{workflowr::wflow_start()}.
#' @param user.email Character. Git user email (optional).
#'   Passed to \code{workflowr::wflow_start()}.
#' @param open Logical. Open the project in RStudio after creation?
#'   Default \code{TRUE} in interactive sessions.
#'
#' @return Invisibly returns the path to the created project directory.
#'
#' @details
#' The function proceeds in two phases:
#'
#' \strong{Phase 1 -- workflowr base:} Calls \code{workflowr::wflow_start()}
#' with \code{git = FALSE} to create the standard workflowr scaffold without
#' committing yet.
#'
#' \strong{Phase 2 -- reflowR theming:} Replaces \code{_site.yml},
#' \code{index.Rmd}, and \code{about.Rmd} with themed versions; adds the
#' analysis pipeline Rmd files, CSS/SCSS, icon, setup script, utility script,
#' Quarto report template, and additional subdirectories. If \code{git = TRUE},
#' initializes git and commits all files.
#'
#' @examples
#' \dontrun{
#' library(reflowR)
#'
#' # Clinical trial project with full pipeline
#' reflow_init(
#'   directory = "~/projects/sepsis_trial",
#'   author    = "Raban Heller",
#'   email     = "raban.heller@outlook.com",
#'   scheme    = "clinical",
#'   depth     = "standard"
#' )
#'
#' # Quick stats project
#' reflow_init(
#'   directory = "~/projects/simulation_study",
#'   author    = "Raban Heller",
#'   scheme    = "code",
#'   depth     = "minimal"
#' )
#'
#' # Then use standard workflowr commands:
#' workflowr::wflow_build()
#' workflowr::wflow_publish("analysis/*.Rmd", message = "Initial build")
#' }
#'
#' @seealso
#' \code{\link[workflowr]{wflow_start}} for the underlying project creation,
#' \code{\link[workflowr]{wflow_build}} to render Rmd files,
#' \code{\link[workflowr]{wflow_publish}} to commit + build + commit,
#' \code{\link{reflow_schemes}} and \code{\link{reflow_presets}} for available options.
#'
#' @export
reflow_init <- function(
  directory,
  name        = NULL,
  author      = "Author Name",
  email       = NULL,
  affiliation = NULL,
  orcid       = NULL,
  website     = NULL,
  scheme      = c("basic", "clinical", "code", "special", "other"),
  depth       = c("standard", "minimal", "extended"),
  seed        = as.integer(format(Sys.Date(), "%Y%m%d")),
  github_url  = NULL,
  existing    = FALSE,
  git         = TRUE,
  change_wd   = rlang::is_interactive(),
  user.name   = NULL,
  user.email  = NULL,
  open        = rlang::is_interactive()
) {
  # --- Phase 1: workflowr base scaffold ---
  scheme <- match.arg(scheme)
  depth  <- match.arg(depth)
  if (is.null(name)) name <- basename(directory)

  cli::cli_h1("Creating reflowR project: {.val {name}}")
  cli::cli_alert_info("Scheme: {.val {scheme}} | Depth: {.val {depth}}")

  # Call workflowr -- git=FALSE so we commit after adding our files
  cli::cli_h2("Phase 1: workflowr scaffold")
  workflowr::wflow_start(
    directory  = directory,
    name       = name,
    git        = FALSE,
    existing   = existing,
    overwrite  = FALSE,
    change_wd  = FALSE,
    user.name  = user.name,
    user.email = user.email
  )

  project_dir <- fs::path_abs(directory)

  # --- Phase 2: reflowR theming ---
  cli::cli_h2("Phase 2: reflowR theming")
  s <- reflow_scheme(scheme)
  p <- reflow_preset(depth)

  # 2a. Create additional directories
  cli::cli_alert("Creating directory structure...")
  dirs_to_create <- c(
    "assets", "data/raw", "data/processed",
    "output/figures", "output/tables", "output/models",
    "docs/assets", "report", "instructions"
  )
  for (d in dirs_to_create) {
    fs::dir_create(fs::path(project_dir, d))
  }

  # 2b. Generate themed _site.yml (replaces workflowr default)
  cli::cli_alert("Generating themed _site.yml...")
  generate_site_yml(project_dir, name = name, scheme = s, preset = p,
                    github_url = github_url, website = website)

  # 2c. Generate CSS and SCSS from scheme
  cli::cli_alert("Generating theme CSS/SCSS...")
  generate_css(s, fs::path(project_dir, "assets", "theme.css"))
  generate_scss(s, fs::path(project_dir, "assets", "_theme.scss"))

  # 2d. Copy scheme icon
  cli::cli_alert("Adding scheme icon...")
  icon_src <- system.file("icons", paste0("icon_", scheme, ".svg"),
                          package = "reflowR")
  fs::file_copy(icon_src, fs::path(project_dir, "assets", "icon.svg"))
  fs::file_copy(icon_src, fs::path(project_dir, "docs", "assets", "icon.svg"))

  # 2e. Update _workflowr.yml with user seed
  cli::cli_alert("Configuring workflowr seed...")
  wf_yml <- fs::path(project_dir, "_workflowr.yml")
  writeLines(c(
    paste0("seed: ", seed),
    'knit_root_dir: "."',
    "workflowr_version: 1.7.2"
  ), wf_yml)

  # 2f. Generate R scripts
  cli::cli_alert("Generating R scripts...")
  generate_setup_script(project_dir, scheme = s)
  generate_utils_script(project_dir, scheme = s)

  # 2g. Generate code library (formulary, data_io, table/plot helpers)
  cli::cli_alert("Generating code library...")
  generate_code_library(project_dir, scheme = s, preset = p)

  # 2h. Generate Rmd pipeline files
  cli::cli_alert("Generating {length(p$files)} analysis Rmd files...")
  for (f in p$files) {
    generate_rmd(project_dir, file_name = f, author = author,
                 description = p$descriptions[[f]], scheme = s, preset = p)
  }

  # 2i. Replace index.Rmd and about.Rmd
  cli::cli_alert("Generating index and about pages...")
  generate_index_rmd(project_dir, name = name, author = author,
                     email = email, scheme = s, preset = p)
  generate_about_rmd(project_dir, name = name, author = author,
                     email = email, affiliation = affiliation,
                     orcid = orcid, website = website, scheme = s, preset = p)

  # 2j. Generate Quarto report
  cli::cli_alert("Generating Quarto report template...")
  generate_report_qmd(project_dir, name = name, author = author,
                      scheme = s, preset = p)

  # 2k. Generate project README
  cli::cli_alert("Generating README...")
  generate_project_readme(project_dir, name = name, author = author,
                          scheme = s, preset = p)

  # --- Phase 3: Git init + first commit (if requested) ---
  if (git) {
    cli::cli_h2("Phase 3: Git initialization")
    cli::cli_alert("Initializing git and making first commit...")
    repo <- git2r::init(project_dir)
    git2r::add(repo, path = ".")
    tryCatch({
      if (!is.null(user.name) && !is.null(user.email)) {
        git2r::config(repo, user.name = user.name, user.email = user.email)
        sig <- git2r::default_signature(repo)
        git2r::commit(repo, message = "Initial reflowR project", author = sig)
      } else {
        git2r::commit(repo, message = "Initial reflowR project")
      }
      cli::cli_alert_success("Initial commit created.")
    }, error = function(e) {
      cli::cli_warn("Git commit failed: {e$message}. You can commit manually.")
    })
  }

  # --- Done ---
  cli::cli_h2("Done!")
  cli::cli_alert_success("Project created at {.path {project_dir}}")
  cli::cli_alert_info("Scheme: {.val {s$label}} | Pipeline: {.val {p$label}}")
  cli::cli_text("")
  cli::cli_alert_info("Next steps:")
  cli::cli_ol(c(
    "Open {.file {name}.Rproj} in RStudio",
    "Place your data in {.file data/raw/}",
    "Run {.code workflowr::wflow_build()} to render",
    "Run {.code workflowr::wflow_publish()} to commit + build"
  ))

  if (change_wd) setwd(project_dir)
  if (open && requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    rstudioapi::openProject(project_dir, newSession = TRUE)
  }

  invisible(project_dir)
}
