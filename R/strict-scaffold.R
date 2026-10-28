strict_template <- function(name) {
  path <- system.file("templates", "strict-v1", name, package = "reflowR")
  if (!nzchar(path) || !file.exists(path)) stop("Canonical strict template unavailable", call. = FALSE)
  paste0(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), "\n")
}

strict_yaml_scalar <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE))

# The stylesheet is part of the canonical scaffold: changing the v5 site CSS
# changes strict-v1 file hashes. Legacy declarations keep their original bytes.
strict_css <- function(profile) {
  if (strict_profile_legacy(profile)) {
    return(paste0(":root { --research-primary: ", profile$primary_color, "; }\n.navbar { border-bottom: 4px solid var(--research-primary); }\nh1, h2, a { color: var(--research-primary); }\n.research-badge { border: 1px solid var(--research-primary); padding: .2em .5em; }\n"))
  }
  paste0(site_css(reflow_scheme(profile$scheme)),
         "\n.research-badge {\n  border: 1px solid var(--cttir-topic);\n  padding: 0.2em 0.5em;\n}\n")
}

strict_stage_rmd <- function(stage, profile, author) {
  title <- paste(gsub("_", " ", tools::file_path_sans_ext(basename(stage$analysis))), collapse = " ")
  header <- c("---", paste0("title: ", strict_yaml_scalar(title)),
              paste0("author: ", strict_yaml_scalar(author)), "output: workflowr::wflow_html", "---", "",
              paste0("**", profile$badge, "** | Domain: ", profile$domain, " | Design: ", profile$design), "")
  if (!stage$applicable) return(paste0(paste(c(header, "Status: Not applicable.", "", paste0("Reason: ", stage$reason), ""), collapse = "\n"), "\n"))
  paste0(paste(c(header, "Status: Planned. This scaffold contains no completed analysis.", "",
                 "```{r strict-start, include=FALSE}", 'source("code/strict-setup.R", local = TRUE)',
                 sprintf('.reflow_state <- strict_stage_begin("%s", %dL)', stage$id, profile$seed), "```", "",
                 "## Objective and assumptions", "", "Record the planned question, inputs, decisions and assumptions.", "",
                 "## Analysis", "", "```{r analysis}", sprintf('source("%s", local = TRUE)', stage$code), "```", "",
                 "## Findings and limitations", "", "Report results only after the reviewed analysis has run.", "",
                 "```{r strict-finish, include=FALSE}", sprintf('strict_stage_finish(.reflow_state, "%s", "%s")', stage$analysis, stage$code), "```", ""), collapse = "\n"), "\n")
}

#' Plan a Strict Workflow Scaffold
#'
#' Returns canonical project files without writing or running them. The supplied
#' dependency lock is parsed data from an existing renv lockfile; no dependencies
#' are installed or restored. Preserve the returned declaration outside the
#' generated project as the trusted input to validation.
#'
#' @param profile A declaration from [reflow_profile()].
#' @param name Plain-text project name.
#' @param author Plain-text project author.
#' @param dependency_lock Parsed R renv lockfile containing R and Packages fields.
#' @param composition For `strict-cttir-v1` only, a plain list with exactly
#'   `schema_version = 1L`, `study`, `recipe`, and `runtime`. Study and recipe are
#'   bounded JSON objects whose scientific semantics are validated by cttiR.
#'   Runtime has `package = "cttiR"`, `version`, `writer = "study_stage"`, and
#'   `files`, a complete named list of relative installed-package file SHA-256
#'   digests. The package version must match the dependency lock. Default `NULL`.
#'
#' @return A list containing the validated profile, trusted declaration, named
#'   relative file contents, directories, and content fingerprints.
#' @export
reflow_scaffold <- function(profile, name, author, dependency_lock, composition = NULL) {
  profile <- strict_profile_canonical(profile)
  name <- strict_text(name, "project name", 160L)
  author <- strict_text(author, "author", 160L)
  lock <- strict_dependency_lock(dependency_lock)
  composed <- identical(profile$profile_version, "strict-cttir-v1")
  if (composed) composition <- strict_cttir_composition(composition, lock)
  else if (!is.null(composition)) stop("Composition requires strict-cttir-v1", call. = FALSE)
  declaration <- list(profile = unclass(profile), name = name, author = author, dependency_lock = lock)
  if (composed) declaration$composition <- composition
  files <- list()
  files[["reflow-profile.json"]] <- strict_json(declaration)
  files[["renv.lock"]] <- strict_json(lock)
  files[[".Rprofile"]] <- strict_template("Rprofile")
  files[[".gitignore"]] <- strict_template("gitignore")
  files[["reflow.Rproj"]] <- strict_template("project.Rproj.in")
  files[["code/strict-setup.R"]] <- strict_template("strict-setup.R")
  if (composed) {
    files[["cttir-study.json"]] <- strict_json(composition$study)
    files[["cttir-recipe.json"]] <- strict_json(composition$recipe)
    files[["cttir-runtime.json"]] <- strict_json(composition$runtime)
    controls <- c(.strict_cttir_controls, "reflow-profile.json", "renv.lock")
    expected_controls <- lapply(files[controls], strict_sha)
    files[["code/strict-setup.R"]] <- paste0(files[["code/strict-setup.R"]], strict_cttir_runtime_text(expected_controls))
  }
  files[["_workflowr.yml"]] <- paste0("seed: ", profile$seed, '\nknit_root_dir: "."\nworkflowr_version: ', lock$Packages$workflowr$Version, "\n")
  links <- unlist(lapply(profile$stages, function(s) c(paste0("    - text: ", strict_yaml_scalar(s$id)), paste0("      href: ", tools::file_path_sans_ext(basename(s$analysis)), ".html"))), use.names = FALSE)
  files[["analysis/_site.yml"]] <- paste0(paste(c(paste0("name: ", strict_yaml_scalar(name)), "output_dir: ../docs", "navbar:",
    paste0("  title: ", strict_yaml_scalar(name)), "  left:", '  - text: "Home"', "    href: index.html", '  - text: "Stages"', "    menu:", links,
    "output:", "  workflowr::wflow_html:", "    toc: true", "    toc_depth: 3", "    theme: flatly", "    css: strict.css", "    code_folding: show"), collapse = "\n"), "\n")
  files[["analysis/strict.css"]] <- strict_css(profile)
  files[["analysis/index.Rmd"]] <- paste0(paste(c("---", paste0("title: ", strict_yaml_scalar(name)), "site: workflowr::wflow_site", "output: workflowr::wflow_html", "---", "",
    paste0("**", profile$badge, "** | Domain: ", profile$domain, " | Design: ", profile$design), "",
    "This is a planned research workflow. No statistical method is mandatory merely because a stage exists.", "",
    vapply(profile$stages, function(s) paste0("- [", s$id, "](", tools::file_path_sans_ext(basename(s$analysis)), ".html): ", if (s$applicable) "planned" else paste0("not applicable: ", s$reason)), character(1)), "",
    "Publication requires a separate, explicit decision after review. Child publications share this repository, dependency lock and site."), collapse = "\n"), "\n")
  files[["README.md"]] <- paste0("# ", name, "\n\nAuthor: ", author, "\n\n", profile$badge, "; ", profile$domain, "; ", profile$design,
    "\n\nUse one Git repository, one renv.lock and one workflowr site. Initialize Git explicitly if needed; creation does not commit or publish. Provision the pinned dependencies explicitly in a reviewed external R library before rendering; project-local activation files and library symlinks are not part of this strict profile. A clean external R session and the declared seed are required.\n\nValidate against the trusted external declaration and reviewed file hashes before executing modified analysis code. Validation reads data and hashes; it never runs project code. Hash approval is not a security sandbox or scientific signoff.\n\nKeep raw inputs read-only and ignored by Git. Record data access, jurisdiction and legal basis before execution. Store processing outputs under output/ and provenance receipts under output/provenance/. Missing or unknown legal declarations block execution readiness.\n\nPublication children belong under publications/ and output/publications/, with reports in this project's analysis/ tree. Do not create nested Git, renv, R project or workflowr roots. Publishing is a separate explicit action.\n")
  files[["data/raw/README.md"]] <- "Raw inputs are read-only and ignored by Git. Record source, permissions and hashes before use. This directory is never an output destination.\n"
  files[["data/README.md"]] <- "Place immutable inputs in raw/. Write derived files under output/. Data access and legal declarations do not replace permissions review.\n"
  files[["output/provenance/README.md"]] <- "Stage receipts record input/output hashes, code hashes, seed and session. Rendering a scaffold does not complete an analysis.\n"
  files[["publications/README.md"]] <- "Each publication is a child of this project. Reuse its Git root, renv.lock, source data and workflowr site; never create nested execution roots.\n"
  files[["renv/README.md"]] <- "The root renv.lock declares the environment. Provision it explicitly in a reviewed external R library before analysis. No automatic activation, install or update runs at startup or during rendering.\n"
  for (stage in profile$stages) {
    files[[stage$analysis]] <- if (composed) strict_cttir_stage_rmd(stage, profile, author) else strict_stage_rmd(stage, profile, author)
    files[[stage$code]] <- if (composed && stage$applicable) {
      sprintf('cttiR::study_stage(stage = "%s", directory = ".")\n', stage$id)
    } else paste0("# ", stage$id, ": planned analysis helper.\n# Add reviewed code only; write derived artifacts under output/.\n")
  }
  if (composed) {
    files[["README.md"]] <- sub(
      "Validate against the trusted external declaration and reviewed file hashes before executing modified analysis code. Validation reads data and hashes; it never runs project code. Hash approval is not a security sandbox or scientific signoff.",
      "Validate against the trusted external declaration before executing the fixed report stages. Validation reads data and hashes; it never runs project code. Hash validation is not a security sandbox or scientific signoff.",
      files[["README.md"]], fixed = TRUE)
    files[["README.md"]] <- paste0(files[["README.md"]],
      "\nThis strict-cttir-v1 composition has immutable study, recipe, runtime and stage files. No reviewed-file override is allowed. cttiR validates scientific recipe semantics; reflowR validates structure and pinned provenance. Each report stage verifies the complete installed cttiR file inventory before calling cttiR::study_stage. Provision the declared external library explicitly and start a clean R session. Rendering presents verified existing analysis outputs; it does not perform the analysis or confer scientific approval.\n")
  }
  hashes <- vapply(files, strict_sha, character(1))
  result <- list(profile = profile, declaration = declaration, files = files,
                 directories = c("analysis", "code/stages", "data/raw", "output/provenance", "output/publications", "docs", "publications", "renv"),
                 sha256 = hashes, template_fingerprint = strict_sha(strict_json(as.list(hashes))))
  class(result) <- "reflow_strict_scaffold"
  result
}

#' Write a Strict Workflow Project
#'
#' Writes a new project from canonical templates, without installing dependencies,
#' committing, rendering or publishing. Existing paths and symbolic links are
#' rejected. Raw data files and their directory are made read-only.
#'
#' @inheritParams reflow_scaffold
#' @param directory New project directory.
#' @param git Whether to initialize an empty Git repository. Default `FALSE`.
#'   No commit is made and Git identity configuration is not changed.
#'
#' @return Invisibly, the scaffold with its absolute project directory.
#' @export
reflow_init_strict <- function(directory, profile, name, author, dependency_lock, git = FALSE, composition = NULL) {
  if (!is.character(directory) || length(directory) != 1L || is.na(directory) || !nzchar(directory))
    stop("directory must name one new path", call. = FALSE)
  if (!is.logical(git) || length(git) != 1L || is.na(git)) stop("git must be logical", call. = FALSE)
  path <- fs::path_abs(directory)
  strict_path_ancestors(path)
  if (file.exists(path) || dir.exists(path)) stop("Strict initialization requires a new directory", call. = FALSE)
  parent <- dirname(path)
  if (!dir.exists(parent)) stop("The parent directory must already exist", call. = FALSE)
  scaffold <- reflow_scaffold(profile, name, author, dependency_lock, composition)
  dir.create(path)
  for (directory in scaffold$directories) dir.create(file.path(path, directory), recursive = TRUE, showWarnings = FALSE)
  for (relative in names(scaffold$files)) {
    target <- file.path(path, relative)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    writeBin(charToRaw(enc2utf8(scaffold$files[[relative]])), target)
  }
  Sys.chmod(file.path(path, "data/raw/README.md"), "0444")
  Sys.chmod(file.path(path, "data/raw"), "0555")
  if (git) git2r::init(path)
  scaffold$directory <- as.character(path)
  invisible(scaffold)
}
