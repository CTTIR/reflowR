#' Initialize a Themed workflowr Project
#'
#' Wraps \code{\link[workflowr]{wflow_start}} to create a standard workflowr
#' project, then extends it with a themed colour scheme, structured Rmd analysis
#' pipeline, utility scripts, and a Quarto report template.
#'
#' @param directory Character. Path where the project will be created.
#'   The existing parent is canonicalized; a linked project root is refused.
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
#'   folder? Missing workflowr files are added and present ones left alone.
#'   Default
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
#' The function generates both phases in a private staging directory on the
#' destination filesystem before publishing any project files:
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
#' writing anything and lists them. These preflight refusals leave the project
#' unchanged. Set \code{overwrite = TRUE} to replace them with the
#' reflowR versions. It also stops before writing anything if one of these
#' files, or the folder it goes in, is read-only. Managed file and directory
#' links are refused. Unrelated data and existing workflowr files outside the
#' themed replacement set are not copied or replaced.
#'
#' A generation error leaves the project unchanged. Publication backs up old
#' files, preserves their observed permission modes, and restores them if a
#' rename fails. If recovery itself fails, a `reflow_init_recovery_error` names
#' the retained backup directory in `recovery_path`; inspect it before retrying.
#' This is synchronous recovery, not a crash-atomic transaction or protection
#' against concurrent writers. ACLs and ownership are not copied.
#'
#' Git operations occur only after successful file publication. Existing staged
#' changes prevent a commit. A failed add/commit restores the previous index
#' and leaves the generated project available for manual commit. Linked Git
#' metadata requires `git = FALSE`. The operation needs a writable existing
#' ancestor for its temporary staging directory.
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
#' # workflowr::wflow_build(project = project)
#' # workflowr::wflow_publish(file.path(project, "analysis", "*.Rmd"),
#' #                         project = project)
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
  reflow_init_arguments(directory, name, author, email, affiliation, orcid,
                        website, seed, github_url, existing, overwrite, git,
                        change_wd, user.name, user.email, open)
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
  directory <- reflow_init_destination(directory)
  if (dir.exists(directory) != existing) {
    cli::cli_abort(if (existing) {
      "Directory does not exist; use {.code existing = FALSE}."
    } else {
      "Directory already exists; use {.code existing = TRUE}."
    })
  }

  # Check every file that Phase 2 writes before anything is written. Files
  # that exist already belong to the user (an existing project or an earlier
  # call) and are replaced only on request.
  written <- c(reflow_init_replaced, reflow_init_added(p))
  reflow_init_guard(directory, written)
  if (git) reflow_init_git_guard(directory)
  expected <- lapply(file.path(directory, written), reflow_init_observe)
  names(expected) <- written
  root_identity <- reflow_init_observe(directory)
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

  # Generate in an owned directory on the destination filesystem. The inner
  # basename remains the requested project's name for workflowr's Rproj file.
  anchor <- dirname(directory)
  while (!dir.exists(anchor)) anchor <- dirname(anchor)
  staging <- tempfile(".reflow-stage-", tmpdir = anchor)
  if (!dir.create(staging, mode = "0700")) cli::cli_abort("Cannot create scaffold staging.", class = "reflow_init_error")
  transaction <- new.env(parent = emptyenv())
  transaction$keep <- FALSE
  transaction$anchor <- anchor
  transaction$anchor_identity <- reflow_init_observe(anchor)
  transaction$staging_identity <- reflow_init_observe(staging)
  on.exit({
    if (!transaction$keep) {
      if (identical(reflow_init_observe(staging), transaction$staging_identity)) {
        unlink(staging, recursive = TRUE)
      } else {
        cli::cli_warn("Staging identity changed; no cleanup performed at {.path {staging}}.")
      }
    }
  }, add = TRUE)
  generated <- file.path(staging, basename(directory))

  # Call workflowr -- git=FALSE so we commit after adding our files
  cli::cli_h2("Phase 1: workflowr scaffold")
  reflow_init_start(
    directory  = generated,
    name       = name,
    git        = FALSE,
    existing   = FALSE,
    overwrite  = FALSE,
    change_wd  = FALSE,
    user.name  = user.name,
    user.email = user.email
  )

  project_dir <- fs::path_abs(generated)

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

  # Publish only generated files. Existing workflowr files outside the themed
  # replacement set retain their bytes; unrelated data is never copied/staged.
  ours <- reflow_init_publish(generated, directory, written, expected,
                              root_identity, staging, transaction)
  project_dir <- fs::path_abs(directory)

  # Git is deliberately after successful publication. A generator/publication
  # error cannot change the user's index or repository metadata.
  if (git) reflow_init_commit(project_dir, ours, staging, user.name, user.email, transaction)

  # --- Done ---
  cli::cli_h2("Done!")
  cli::cli_alert_success("Project created at {.path {project_dir}}")
  cli::cli_alert_info("Scheme: {.val {s$label}} | Pipeline: {.val {p$label}}")
  cli::cli_text("")
  cli::cli_alert_info("Next steps:")
  project_file <- paste0(basename(project_dir), ".Rproj")
  project_literal <- encodeString(as.character(project_dir), quote = '"')
  build_command <- paste0("workflowr::wflow_build(project = ", project_literal, ")")
  analysis_literal <- encodeString(file.path(project_dir, "analysis", "*.Rmd"),
                                   quote = '"')
  publish_command <- paste0(
    "workflowr::wflow_publish(", analysis_literal, ', message = "Build", project = ',
    project_literal, ")"
  )
  cli::cli_ol(c(
    "Open {.file {project_file}} in RStudio",
    "Place your data in {.file data/raw/}",
    "Render and publish with the commands below"
  ))
  # Verbatim code keeps long quoted paths intact at narrow console widths.
  cli::cli_code(c(build_command, publish_command))

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
# Linked folders are listed as links, not followed. A folder that does not
# exist yet holds no files.
reflow_init_files <- function(directory) {
  if (!dir.exists(directory)) return(character())
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

reflow_init_arguments <- function(directory, name, author, email, affiliation,
                                  orcid, website, seed, github_url, existing,
                                  overwrite, git, change_wd, user_name,
                                  user_email, open) {
  text <- function(x, label, optional = FALSE) {
    if (optional && is.null(x)) return(invisible(NULL))
    if (!is.character(x) || !is.null(dim(x)) || length(x) != 1L || is.na(x) ||
        !nzchar(trimws(x)) || grepl("[[:cntrl:]]", x)) {
      cli::cli_abort("{label} must be one nonempty character string without control characters.")
    }
  }
  text(directory, "directory")
  text(author, "author")
  optional <- list(name = name, email = email, affiliation = affiliation,
                   orcid = orcid, website = website, github_url = github_url,
                   user.name = user_name, user.email = user_email)
  for (label in names(optional)) text(optional[[label]], label, TRUE)
  flags <- list(existing = existing, overwrite = overwrite, git = git,
                change_wd = change_wd, open = open)
  for (label in names(flags)) {
    x <- flags[[label]]
    if (!is.logical(x) || !is.null(dim(x)) || length(x) != 1L || is.na(x)) {
      cli::cli_abort("{label} must be TRUE or FALSE.")
    }
  }
  if (!is.numeric(seed) || is.complex(seed) || !is.null(dim(seed)) || length(seed) != 1L ||
      is.na(seed) || !is.finite(seed) || seed < 0 ||
      seed > .Machine$integer.max || seed != trunc(seed)) {
    cli::cli_abort("seed must be one nonnegative representable integer.")
  }
  if (is.null(user_name) != is.null(user_email)) {
    cli::cli_abort("Supply both user.name and user.email, or neither.")
  }
}

reflow_init_exists <- function(path) {
  !is.na(fs::file_info(path, follow = FALSE, fail = FALSE)$type)
}

reflow_init_destination <- function(directory) {
  path <- as.character(fs::path_abs(directory))
  if (isTRUE(unname(fs::is_link(path)))) {
    cli::cli_abort("The project root must not be a symbolic link: {.path {path}}.")
  }
  # Canonicalize the caller-selected parent, including system aliases such as
  # macOS /var. The root and every managed component below it remain checked.
  parent <- dirname(path)
  suffix <- basename(path)
  while (!dir.exists(parent)) {
    if (reflow_init_exists(parent)) {
      cli::cli_abort("The project parent is not an accessible directory.")
    }
    suffix <- file.path(basename(parent), suffix)
    next_parent <- dirname(parent)
    if (identical(parent, next_parent)) cli::cli_abort("Cannot resolve project parent.", class = "reflow_init_error")
    parent <- next_parent
  }
  as.character(fs::path(fs::path_real(parent), suffix))
}

reflow_init_guard <- function(root, relative, directories = FALSE) {
  paths <- c(root, file.path(root, relative))
  for (path in paths) {
    leaf <- TRUE
    repeat {
      info <- fs::file_info(path, follow = FALSE, fail = FALSE)
      if (isTRUE(unname(fs::is_link(path)))) {
        cli::cli_abort("Managed scaffold paths must not be symbolic links: {.path {path}}.")
      }
      if (!is.na(info$type)) {
        want <- if (identical(path, root) || !leaf || directories) "directory" else "file"
        if (as.character(info$type) != want) {
          cli::cli_abort("Scaffold destination has an incompatible type: {.path {path}}.")
        }
      }
      if (identical(path, root)) break
      path <- dirname(path)
      leaf <- FALSE
    }
  }
  invisible(TRUE)
}

reflow_init_observe <- function(path) {
  info <- fs::file_info(path, follow = FALSE, fail = FALSE)
  if (is.na(info$type)) return(NULL)
  out <- list(type = as.character(info$type), inode = as.character(info$inode),
               device = as.character(info$device_id),
               mode = as.integer(file.info(path)$mode))
  if (out$type == "file") {
    out$hash <- digest::digest(file = path, algo = "sha256")
  }
  out
}

reflow_init_rename <- function(from, to) file.rename(from, to)

reflow_init_publish <- function(generated, directory, written, expected,
                                root_identity, staging, transaction) {
  staging_guard <- function() {
    if (!identical(reflow_init_observe(transaction$anchor),
                   transaction$anchor_identity) ||
        !identical(reflow_init_observe(staging), transaction$staging_identity)) {
      cli::cli_abort("The scaffold parent or staging directory changed before publication.")
    }
  }
  staging_guard()
  files <- as.character(fs::path_rel(
    fs::dir_ls(generated, recurse = TRUE, all = TRUE, type = "file"), generated
  ))
  if (!all(written %in% files)) {
    cli::cli_abort("Scaffold generation did not produce every required file.", class = "reflow_init_error")
  }
  reflow_init_guard(directory, files)
  if (!identical(reflow_init_observe(directory), root_identity)) {
    cli::cli_abort("The project root changed during scaffold generation.", class = "reflow_init_error")
  }
  for (name in written) {
    if (!identical(reflow_init_observe(file.path(directory, name)), expected[[name]])) {
      cli::cli_abort("A managed file changed during scaffold generation: {name}.",
                     class = "reflow_init_error")
    }
  }
  # workflowr's existing=TRUE preserves its already present files. Only the
  # explicit themed replacement set may replace a caller-owned file.
  files <- files[files %in% written | !file.exists(file.path(directory, files))]
  previous <- lapply(file.path(directory, files), reflow_init_observe)
  dirs <- as.character(fs::path_rel(
    fs::dir_ls(generated, recurse = TRUE, all = TRUE, type = "directory"), generated
  ))
  reflow_init_guard(directory, dirs, directories = TRUE)
  backups <- file.path(staging, "backup")
  dir.create(backups)
  applied <- list()
  directory_state <- new.env(parent = emptyenv())
  directory_state$created <- list()
  complete <- FALSE
  on.exit({
    if (!complete) {
      failed <- character()
      for (entry in rev(applied)) {
        target <- file.path(directory, entry$name)
        restored <- tryCatch({
          staging_guard()
          reflow_init_guard(directory, entry$name)
          if (!is.null(entry$published)) {
            if (!identical(reflow_init_observe(target), entry$published)) {
              cli::cli_abort("Published file changed during recovery.", class = "reflow_init_error")
            }
            if (!file.remove(target)) cli::cli_abort("Cannot remove the staged replacement.", class = "reflow_init_error")
          }
          if (entry$backed) {
            if (reflow_init_exists(target) ||
                !reflow_init_rename(entry$backup, target)) {
              cli::cli_abort("Cannot restore the previous file.", class = "reflow_init_error")
            }
          }
          TRUE
        }, error = function(e) FALSE)
        if (!restored) failed <- c(failed, entry$name)
      }
      for (entry in rev(directory_state$created)) {
        path <- entry$path
        removed <- tryCatch({
          staging_guard()
          if (!identical(reflow_init_observe(path), entry$identity)) {
            cli::cli_abort("Created directory changed during recovery.", class = "reflow_init_error")
          }
          if (isTRUE(unname(fs::is_link(path)))) cli::cli_abort("Directory became a link.", class = "reflow_init_error")
          children <- fs::dir_ls(path, all = TRUE)
          if (length(children)) cli::cli_abort("Directory is no longer empty.", class = "reflow_init_error")
          unlink(path, recursive = TRUE) == 0L
        }, error = function(e) FALSE)
        if (!removed) failed <- c(failed, path)
      }
      if (length(failed)) {
        transaction$keep <- TRUE
        cli::cli_abort(c(
          "Scaffold recovery could not complete; backups remain at {.path {staging}}.",
          "i" = "Inspect {.val {failed}} before retrying."
        ), class = "reflow_init_recovery_error", recovery_path = staging)
      }
    }
  }, add = TRUE)
  make_directory <- function(path) {
    staging_guard()
    missing <- character()
    while (!dir.exists(path)) {
      if (reflow_init_exists(path)) cli::cli_abort("A scaffold directory is occupied.", class = "reflow_init_error")
      missing <- c(path, missing)
      path <- dirname(path)
    }
    for (path in missing) {
      if (!dir.create(path, mode = "0755")) cli::cli_abort("Cannot create scaffold directory.", class = "reflow_init_error")
      directory_state$created[[length(directory_state$created) + 1L]] <- list(
        path = path, identity = reflow_init_observe(path)
      )
    }
  }
  make_directory(directory)
  for (name in dirs) make_directory(file.path(directory, name))
  for (i in seq_along(files)) {
    staging_guard()
    name <- files[[i]]
    target <- file.path(directory, name)
    source <- file.path(generated, name)
    reflow_init_guard(directory, name)
    if (!identical(reflow_init_observe(target), previous[[i]])) {
      cli::cli_abort("A scaffold destination changed before publication: {name}.",
                     class = "reflow_init_error")
    }
    if (!is.null(previous[[i]])) {
      mode <- previous[[i]]$mode
      if (!Sys.chmod(source, as.octmode(mode), use_umask = FALSE) ||
          as.integer(file.info(source)$mode) != mode) {
        cli::cli_abort("Cannot preserve scaffold file permissions: {name}.",
                       class = "reflow_init_error")
      }
    }
    entry <- list(name = name, backup = file.path(backups, as.character(i)),
                  backed = FALSE, published = NULL)
    applied[[i]] <- entry
    if (!is.null(previous[[i]])) {
      if (!reflow_init_rename(target, entry$backup)) cli::cli_abort("Cannot back up scaffold file: {name}.",
                                                  class = "reflow_init_error")
      applied[[i]]$backed <- TRUE
    }
    if (!reflow_init_rename(source, target)) cli::cli_abort("Cannot publish scaffold file: {name}.",
                                                  class = "reflow_init_error")
    applied[[i]]$published <- reflow_init_observe(target)
  }
  complete <- TRUE
  files
}

reflow_init_git_guard <- function(directory) {
  metadata <- file.path(directory, ".git")
  if (reflow_init_exists(metadata) &&
      (isTRUE(unname(fs::is_link(metadata))) || !dir.exists(metadata))) {
    cli::cli_abort("Git metadata must be a local directory; linked worktrees require {.code git = FALSE}.")
  }
  for (file in c("index", "config", "HEAD")) {
    path <- file.path(metadata, file)
    if (isTRUE(unname(fs::is_link(path)))) {
      cli::cli_abort("Git metadata must not contain linked control files.")
    }
  }
}

reflow_init_commit <- function(directory, files, staging, user_name, user_email,
                               transaction) {
  reflow_init_git_guard(directory)
  repo <- git2r::init(directory)
  staged <- git2r::status(repo, unstaged = FALSE, untracked = FALSE)$staged
  if (length(staged)) {
    cli::cli_warn(c(
      "The repository in {.path {directory}} has staged changes; no commit was made.",
      "i" = "Commit the project files yourself."
    ))
    return(invisible(NULL))
  }
  index <- file.path(directory, ".git", "index")
  backup <- file.path(staging, "git-index")
  had_index <- file.exists(index)
  if (had_index) fs::file_copy(index, backup)
  committed <- FALSE
  tryCatch({
    if (!is.null(user_name)) {
      git2r::config(repo, user.name = user_name, user.email = user_email)
    }
    owd <- setwd(directory)
    tryCatch(git2r::add(repo, path = files), finally = setwd(owd))
    git2r::commit(repo, message = "Initial reflowR project")
    committed <- TRUE
  }, error = function(e) {
    restored <- tryCatch({
      reflow_init_git_guard(directory)
      if (had_index) {
        fs::file_copy(backup, index, overwrite = TRUE)
        identical(digest::digest(file = backup), digest::digest(file = index))
      } else {
        !file.exists(index) || file.remove(index)
      }
    }, error = function(error) FALSE)
    if (!restored) {
      transaction$keep <- TRUE
      cli::cli_abort(
        "Git index recovery failed; the prior index is retained at {.path {backup}}.",
        class = "reflow_init_recovery_error", recovery_path = staging,
        parent = e
      )
    }
    cli::cli_warn("Git commit failed: {conditionMessage(e)}. The previous index was restored; commit the generated project manually.")
  })
  if (committed) cli::cli_alert_success("Initial commit created.")
  invisible(NULL)
}
