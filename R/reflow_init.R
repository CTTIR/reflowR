#' Initialize a Themed workflowr Project
#'
#' Wraps \code{\link[workflowr]{wflow_start}} to create a standard workflowr
#' project, then extends it with a themed colour scheme, structured Rmd analysis
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
#' @param scheme Character. CTTIR v5 family whose colours, icon and fonts
#'   style the site: one of \code{"brand"} (default), \code{"spectral"},
#'   \code{"spatial"}, \code{"omics"}, \code{"clinical"}, \code{"methods"},
#'   \code{"simulation"}, \code{"evidence"} or \code{"platform"}. The earlier
#'   names \code{"basic"}, \code{"code"}, \code{"special"} and \code{"other"}
#'   remain accepted as aliases of brand, methods, evidence and platform.
#'   See \code{\link{reflow_schemes}}.
#' @param depth Character. Rmd pipeline depth. One of \code{"minimal"},
#'   \code{"standard"}, \code{"extended"}. Defaults to \code{"standard"}.
#' @param seed Integer. Random seed for workflowr reproducibility.
#'   Defaults to \code{as.integer(format(Sys.Date(), "\%Y\%m\%d"))}.
#' @param github_url Character. GitHub repository URL (optional).
#'   Adds a GitHub link to the navbar.
#' @param existing Logical. Is the \code{directory} an existing project or
#'   folder? Passed to \code{workflowr::wflow_start(existing = ...)}, which adds
#'   the missing workflowr files and leaves present ones alone. Default
#'   \code{FALSE}. Files that reflowR writes are replaced only with
#'   \code{overwrite = TRUE}. Files already in the folder are not committed
#'   (see \code{git}).
#' @param overwrite Logical. Replace files that reflowR writes if they already
#'   exist in \code{directory}: \code{README.md}, \code{_workflowr.yml},
#'   \code{analysis/_site.yml}, \code{analysis/index.Rmd},
#'   \code{analysis/about.Rmd} and the theme, icon, script, pipeline and report
#'   files. Default \code{FALSE}: the function then stops before writing
#'   anything and lists the files. Other workflowr files are never replaced.
#'   Re-theming at a different \code{depth} leaves the pipeline files of the
#'   earlier depth that the new one does not write in \code{analysis/}; remove
#'   them yourself if they are no longer needed.
#' @param git Logical. Initialize a git repository (or use the one in an
#'   existing folder) and commit the files that this call writes? Other files
#'   in \code{directory}, and changes to them, are not committed. If the
#'   repository already has staged changes, no commit is made and a warning
#'   says so. Default \code{TRUE}.
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
#' \code{index.Rmd}, \code{about.Rmd}, \code{_workflowr.yml} and
#' \code{README.md} with themed versions; adds the
#' analysis pipeline Rmd files, CSS/SCSS, icon, setup script, utility script,
#' Quarto report template, and additional subdirectories. If \code{git = TRUE},
#' initializes git and commits the files written by both phases.
#'
#' If any of the files that Phase 2 writes already exists in \code{directory}
#' before the call, for example the README or \code{_site.yml} of an existing
#' workflowr project or the files of an earlier call, the function stops before
#' writing anything and lists them, so a project is never changed silently or
#' left partly updated. Set \code{overwrite = TRUE} to replace them with the
#' reflowR versions. It also stops before writing anything if one of these
#' files, or the folder it goes in, is read-only.
#'
#' @examples
#' # A clinical project with the three-step pipeline, in a temporary folder
#' project <- file.path(tempdir(), "my_project")
#' reflow_init(
#'   directory = project,
#'   author    = "Your Name",
#'   email     = "you@example.com",
#'   scheme    = "clinical",
#'   depth     = "minimal",
#'   git       = FALSE,
#'   change_wd = FALSE,
#'   open      = FALSE
#' )
#' list.files(file.path(project, "analysis"))
#'
#' # In a real project, continue with the standard workflowr commands, e.g.
#' # workflowr::wflow_build() and workflowr::wflow_publish().
#' unlink(project, recursive = TRUE)
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
  scheme      = c("brand", "spectral", "spatial", "omics", "clinical",
                  "methods", "simulation", "evidence", "platform",
                  "basic", "code", "special", "other"),
  depth       = c("standard", "minimal", "extended"),
  seed        = as.integer(format(Sys.Date(), "%Y%m%d")),
  github_url  = NULL,
  existing    = FALSE,
  overwrite   = FALSE,
  git         = TRUE,
  change_wd   = rlang::is_interactive(),
  user.name   = NULL,
  user.email  = NULL,
  open        = rlang::is_interactive()
) {
  # On Windows, base R and fs expand "~" to different folders; expand it once
  # so that the checks, workflowr and Phase 2 all use the same one.
  directory <- path.expand(directory)

  # --- Phase 1: workflowr base scaffold ---
  scheme <- match.arg(scheme)
  depth  <- match.arg(depth)
  if (is.null(name)) name <- basename(directory)
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    cli::cli_abort("{.arg overwrite} must be {.code TRUE} or {.code FALSE}.")
  }
  p <- reflow_preset(depth)

  # Check every file that Phase 2 writes before anything is written. Files
  # that exist already belong to the user (an existing project or an earlier
  # call) and are replaced only on request.
  written <- c(reflow_init_replaced, reflow_init_added(p))
  present <- written[file.exists(file.path(directory, written))]
  if (length(present) && !overwrite) {
    cli::cli_abort(c(
      "Files that reflowR writes already exist in {.path {directory}}; nothing was changed.",
      "i" = "Move or remove {.file {present}}, or set {.code overwrite = TRUE} to replace them."
    ))
  }
  # A file that cannot be written would stop the call halfway and leave the
  # project partly re-themed. A folder that does not exist yet holds no files;
  # workflowr creates it, or stops before anything is written.
  blocked <- if (dir.exists(directory)) {
    written[!vapply(file.path(directory, written), reflow_init_writable, logical(1))]
  } else {
    character()
  }
  if (length(blocked)) {
    cli::cli_abort(c(
      "reflowR cannot write all of its files in {.path {directory}}; nothing was changed.",
      "i" = "Make {.file {blocked}} or the folders they go in writable."
    ))
  }

  cli::cli_h1("Creating reflowR project: {.val {name}}")
  cli::cli_alert_info("Scheme: {.val {scheme}} | Depth: {.val {depth}}")
  if (length(present)) cli::cli_alert_warning("Replacing {.file {present}}.")

  # Files that are there before the call belong to the user; the first
  # commit takes only what wflow_start() and Phase 2 write.
  before <- if (git) reflow_init_files(directory) else character()

  # Call workflowr -- git=FALSE so we commit after adding our files
  cli::cli_h2("Phase 1: workflowr scaffold")
  reflow_init_start(
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
  # The site stylesheet lives next to the pages so that workflowr copies it
  # into docs/; a link outside docs/ would break once the site is published.
  generate_css(s, fs::path(project_dir, "analysis", "theme.css"))
  generate_scss(s, fs::path(project_dir, "assets", "_theme.scss"))

  # 2d. Copy the family icon (aliases resolve to their v5 family)
  cli::cli_alert("Adding scheme icon...")
  icon_src <- system.file("icons", s$navbar_icon, package = "reflowR")
  fs::file_copy(icon_src, fs::path(project_dir, "assets", "icon.svg"),
                overwrite = overwrite)
  fs::file_copy(icon_src, fs::path(project_dir, "docs", "assets", "icon.svg"),
                overwrite = overwrite)

  # 2e. Update _workflowr.yml with user seed
  cli::cli_alert("Configuring workflowr seed...")
  wf_yml <- fs::path(project_dir, "_workflowr.yml")
  writeLines(c(
    paste0("seed: ", seed),
    'knit_root_dir: "."',
    paste0("workflowr_version: ", reflow_init_workflowr_version())
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
    ours <- union(setdiff(reflow_init_files(project_dir), before), written)
    # A commit takes the whole index, so changes the user has already staged
    # would end up in it.
    staged <- git2r::status(repo, unstaged = FALSE, untracked = FALSE)$staged
    if (length(staged)) {
      cli::cli_warn(c(
        "The repository in {.path {project_dir}} has staged changes; no commit was made.",
        "i" = "Commit the project files yourself."
      ))
    } else {
      # The paths are relative to the project root, which is the working
      # directory while git2r resolves them.
      owd <- setwd(project_dir)
      tryCatch(git2r::add(repo, path = ours), finally = setwd(owd))
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

# workflowr files that Phase 2 of reflow_init() replaces with themed versions,
# relative to the project directory.
reflow_init_replaced <- c(
  "README.md", "_workflowr.yml",
  file.path("analysis", c("_site.yml", "index.Rmd", "about.Rmd"))
)

# Whether a file can be written: an existing one must be writable, a new one
# needs a writable folder as its nearest existing ancestor.
reflow_init_writable <- function(path) {
  if (file.exists(path)) {
    return(!dir.exists(path) && file.access(path, 2) == 0)
  }
  repeat {
    parent <- dirname(path)
    if (identical(parent, path) || file.exists(parent)) break
    path <- parent
  }
  dir.exists(parent) && file.access(parent, 2) == 0
}

# The workflowr scaffold and the workflowr version written to _workflowr.yml,
# as package functions so that tests can replace them.
reflow_init_start <- function(...) workflowr::wflow_start(...)
reflow_init_workflowr_version <- function() utils::packageVersion("workflowr")

# Files in a project folder, relative to it, leaving out the .git folder.
# Linked folders are listed as links, not followed.
reflow_init_files <- function(directory) {
  files <- fs::dir_ls(directory, recurse = TRUE, all = TRUE,
                      type = c("file", "symlink"), fail = FALSE)
  files <- as.character(fs::path_rel(files, directory))
  files[files != ".git" & !startsWith(files, ".git/")]
}

# Files that Phase 2 of reflow_init() adds on top of the workflowr scaffold,
# relative to the project directory.
reflow_init_added <- function(preset) {
  c(
    file.path("analysis", c("theme.css", paste0(preset$files, ".Rmd"))),
    file.path("assets", c("_theme.scss", "icon.svg")),
    file.path("docs", "assets", "icon.svg"),
    file.path("code", c("00_setup.R", "utils.R", "formulary.R", "data_io.R",
                        "table_helpers.R", "plot_helpers.R")),
    file.path("report", "report.qmd")
  )
}
