strict_path_ancestors <- function(path) {
  path <- as.character(fs::path_abs(path))
  repeat {
    link <- Sys.readlink(path)
    if (!is.na(link) && nzchar(link)) stop("Symbolic links are not allowed: ", path, call. = FALSE)
    parent <- dirname(path)
    if (identical(parent, path)) break
    path <- parent
  }
  invisible(TRUE)
}

strict_tree_files <- function(root) {
  visit <- function(path) {
    link <- Sys.readlink(path)
    if (!is.na(link) && nzchar(link)) stop("Symbolic links are not allowed: ", path, call. = FALSE)
    if (!dir.exists(path)) return(path)
    children <- list.files(path, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    unlist(lapply(children, visit), use.names = FALSE)
  }
  strict_path_ancestors(root)
  visit(root)
}

strict_relative_paths <- function(paths) {
  if (!is.character(paths) || anyNA(paths) || anyDuplicated(paths) ||
      any(!nzchar(paths)) || any(grepl("[\\\\\r\n\t]", paths)) ||
      any(grepl("^/|^[A-Za-z]:|(^|/)\\.\\.?(/|$)|//|/$", paths)))
    stop("Expected unique, normalized relative paths", call. = FALSE)
  paths
}

strict_review_hashes <- function(reviewed_files) {
  if (is.null(reviewed_files)) return(stats::setNames(character(), character()))
  if (is.list(reviewed_files)) reviewed_files <- unlist(reviewed_files, use.names = TRUE)
  if (!is.character(reviewed_files) || is.null(names(reviewed_files)) || anyNA(reviewed_files) ||
      any(!grepl("^[a-f0-9]{64}$", reviewed_files)))
    stop("Reviewed files must map paths to SHA-256 digests", call. = FALSE)
  strict_relative_paths(names(reviewed_files))
  reviewed_files
}

strict_rmd_envelope <- function(text, canonical) {
  split <- function(x) strsplit(x, "\n", fixed = TRUE)[[1L]]
  lines <- split(text); original <- split(canonical)
  header_end <- which(original == "---")[[2L]]
  if (length(lines) < header_end || !identical(lines[seq_len(header_end)], original[seq_len(header_end)])) return(FALSE)
  for (label in c("strict-start", "strict-finish")) {
    chunk <- function(x) {
      start <- which(startsWith(x, paste0("```{r ", label, ",")))
      if (length(start) != 1L) return(NULL)
      ends <- which(seq_along(x) > start & x == "```")
      if (!length(ends)) return(NULL)
      x[start:ends[[1L]]]
    }
    if (!identical(chunk(lines), chunk(original))) return(FALSE)
  }
  required <- c("## Objective and assumptions", "## Analysis", "## Findings and limitations")
  all(required %in% lines) && any(lines == original[startsWith(original, "**")][[1L]])
}

#' Validate a Strict Project Against an External Declaration
#'
#' Reads files without executing project code. The declaration and reviewed hashes
#' must come from a trusted location outside the generated project. A project file
#' cannot approve itself. Static validity, execution readiness and recorded
#' completion are separate results; none constitutes scientific or legal approval.
#'
#' @param directory Existing strict project directory.
#' @param declaration The `declaration` returned by [reflow_scaffold()], retained
#'   externally. Contains profile, name, author, dependency_lock and, only for
#'   `strict-cttir-v1`, the exact composition.
#' @param reviewed_files Optional named character vector mapping declared stage
#'   Rmd/R paths to externally reviewed SHA-256 hashes. Control files are immutable.
#'   The composed cttiR profile refuses all reviewed-file overrides.
#' @param execution_receipts Optional named character vector mapping stage IDs to
#'   externally retained SHA-256 hashes of their execution receipts.
#'
#' @return A list with `valid`, `pristine`, `execution_ready`, `completed`,
#'   per-stage results and diagnostic checks. Structural errors return `valid=FALSE`.
#' @export
reflow_validate_strict <- function(directory, declaration, reviewed_files = NULL,
                                   execution_receipts = NULL) {
  composed <- identical(declaration$profile$profile_version, "strict-cttir-v1")
  strict_names(declaration, c("profile", "name", "author", "dependency_lock", if (composed) "composition"), "trusted declaration")
  expected <- reflow_scaffold(declaration$profile, declaration$name, declaration$author,
                              declaration$dependency_lock, declaration$composition)
  reviewed <- strict_review_hashes(reviewed_files)
  if (composed && length(reviewed)) stop("The composed cttiR profile forbids reviewed-file overrides", call. = FALSE)
  receipt_hashes <- strict_review_hashes(execution_receipts)
  editable <- unlist(lapply(expected$profile$stages, function(s) if (s$applicable) c(s$analysis, s$code)), use.names = FALSE)
  if (any(!names(reviewed) %in% editable)) stop("Only declared applicable stage files may be reviewed", call. = FALSE)
  if (any(!names(receipt_hashes) %in% .strict_stage_ids)) stop("Unknown execution receipt stage", call. = FALSE)
  root <- as.character(fs::path_abs(directory))
  checks <- list()
  add <- function(id, ok, detail = "") checks[[length(checks) + 1L]] <<- list(check = id, pass = isTRUE(ok), detail = detail)
  if (!dir.exists(root)) return(list(valid = FALSE, pristine = FALSE, execution_ready = FALSE, completed = FALSE,
                                   checks = list(list(check = "project_exists", pass = FALSE))))
  paths <- tryCatch(strict_tree_files(root), error = function(e) e)
  if (inherits(paths, "error")) return(list(valid = FALSE, pristine = FALSE, execution_ready = FALSE, completed = FALSE,
                                          checks = list(list(check = "no_symbolic_links", pass = FALSE, detail = conditionMessage(paths)))))
  relative <- substring(paths, nchar(root) + 2L)
  add("normalized_paths", !anyDuplicated(relative))
  nested <- grepl("(^|/)([.]git|renv[.]lock|[.]Rprofile|_workflowr[.]yml)(/|$)|[.]Rproj$", relative)
  allowed_root <- relative %in% c("renv.lock", ".Rprofile", "_workflowr.yml", "reflow.Rproj") | startsWith(relative, ".git/")
  add("single_project_root", !any(nested & !allowed_root))
  actual <- stats::setNames(vapply(paths, function(p) digest::digest(file = p, algo = "sha256"), character(1)), relative)
  changed <- character()
  for (name in names(expected$files)) {
    present <- name %in% names(actual)
    canonical <- present && identical(unname(actual[[name]]), unname(expected$sha256[[name]]))
    approved <- present && name %in% names(reviewed) && identical(unname(actual[[name]]), unname(reviewed[[name]]))
    add(paste0("file:", name), canonical || approved)
    if (!canonical) changed <- c(changed, name)
    if (present && approved && endsWith(name, ".Rmd")) {
      text <- paste0(paste(readLines(file.path(root, name), warn = FALSE), collapse = "\n"), "\n")
      add(paste0("stage_envelope:", name), strict_rmd_envelope(text, expected$files[[name]]))
    }
  }
  add("reviewed_files_present", all(names(reviewed) %in% names(actual)))
  extra <- setdiff(relative, names(expected$files))
  allowed_extra <- grepl("^(data/raw/|output/|docs/|publications/|[.]git/|renv/)", extra)
  executable <- grepl("(^|/)([.]Rprofile|[.]Renviron|Makefile|_targets[.]R)$|[.](R|r|Rmd|rmd|qmd|py|sh|bash|zsh|bat|cmd|exe)$", extra)
  executable <- executable | (startsWith(extra, ".git/hooks/") & !endsWith(extra, ".sample"))
  executable <- executable | (bitwAnd(as.integer(file.info(file.path(root, extra))$mode), 73L) != 0L & !endsWith(extra, ".sample"))
  # Generated site JavaScript is inert to R; arbitrary extra analysis entrypoints are refused.
  add("declared_entrypoints_only", all(allowed_extra & !executable))
  raw <- c(list.dirs(file.path(root, "data/raw"), recursive = TRUE, full.names = TRUE),
           paths[startsWith(relative, "data/raw/")])
  add("raw_inputs_read_only", !length(raw) || all(bitwAnd(as.integer(file.info(raw)$mode), 146L) == 0L))
  valid <- all(vapply(checks, `[[`, logical(1), "pass"))
  profile <- expected$profile
  runtime <- if (composed) strict_cttir_runtime_installed(expected$declaration$composition$runtime) else NULL
  legal_declared <- all(c(profile$data_access, profile$jurisdiction, profile$legal_basis) != "unknown")
  stages <- lapply(profile$stages, function(stage) {
    implemented <- !stage$applicable || composed && valid || all(c(stage$analysis, stage$code) %in% names(reviewed)) &&
      stage$code %in% changed && stage$analysis %in% changed
    complete <- !stage$applicable
    problems <- character()
    if (stage$applicable && stage$id %in% names(receipt_hashes)) {
      name <- paste0("output/provenance/", stage$id, ".json")
      if (!name %in% names(actual) || !identical(unname(actual[[name]]), unname(receipt_hashes[[stage$id]]))) {
        problems <- "Receipt hash differs from trusted external receipt"
      } else {
        receipt <- tryCatch(jsonlite::read_json(file.path(root, name), simplifyVector = FALSE), error = function(e) NULL)
        if (is.null(receipt) || !identical(receipt$stage, stage$id) || !identical(as.numeric(receipt$seed), as.numeric(profile$seed)) ||
            !is.list(receipt$code) || !is.list(receipt$inputs) || !is.list(receipt$outputs) || !length(receipt$outputs)) {
          problems <- "Invalid receipt identity or missing output provenance"
        } else {
          expected_code <- c(stage$analysis, stage$code, "code/strict-setup.R", "renv.lock", "reflow-profile.json")
          if (composed) {
            expected_code <- c(expected_code, .strict_cttir_controls)
            if (!identical(receipt$composition, "strict-cttir-v1")) problems <- c(problems, "Receipt composition differs")
          }
          if (!setequal(names(receipt$code), expected_code)) problems <- c(problems, "Receipt code inventory differs")
          if (!setequal(names(receipt$inputs), relative[startsWith(relative, "data/raw/")])) problems <- c(problems, "Receipt input inventory differs")
          for (group in c("code", "inputs", "outputs")) {
            values <- tryCatch(strict_review_hashes(receipt[[group]]), error = function(e) NULL)
            if (is.null(values) || any(!names(values) %in% names(actual)) || any(actual[names(values)] != values))
              problems <- c(problems, paste("Stale or invalid", group, "hashes"))
            if (group == "outputs" && !is.null(values) && any(!startsWith(names(values), "output/") | startsWith(names(values), "output/provenance/")))
              problems <- c(problems, "Outputs must be derived artifacts, not receipts")
          }
        }
      }
      complete <- implemented && !length(problems)
    }
    list(id = stage$id, applicable = stage$applicable, implemented_and_reviewed = implemented,
         recorded_complete = complete, receipt_errors = problems)
  })
  ready <- valid && legal_declared && (!composed || isTRUE(runtime$current)) &&
    all(vapply(stages, `[[`, logical(1), "implemented_and_reviewed"))
  result <- list(valid = valid, pristine = valid && !length(changed), execution_ready = ready,
       completed = ready && all(vapply(stages, `[[`, logical(1), "recorded_complete")),
       legal_declarations_complete = legal_declared, stages = stages, checks = checks,
       template_fingerprint = expected$template_fingerprint,
       scope = "Static structure and externally pinned provenance; not a security sandbox, scientific acceptance or legal approval")
  if (composed) result$runtime <- runtime
  result
}
