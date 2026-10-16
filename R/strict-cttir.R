.strict_cttir_controls <- c("cttir-study.json", "cttir-recipe.json", "cttir-runtime.json")

strict_cttir_json <- function(x, label) {
  walk <- function(value, depth = 0L) {
    if (depth > 16L || is.object(value) || !is.null(dim(value)) || length(value) > 10000L)
      stop("Invalid or oversized plain JSON in ", label, call. = FALSE)
    if (is.null(value)) return(invisible(NULL))
    if (is.list(value)) {
      keys <- names(value)
      if (!is.null(keys) && (anyNA(keys) || any(!nzchar(keys)) || anyDuplicated(keys) ||
                            any(nchar(keys) > 160L)))
        stop("Invalid JSON object keys in ", label, call. = FALSE)
      for (element in value) walk(element, depth + 1L)
    } else if (!typeof(value) %in% c("character", "logical", "integer", "double") ||
               !is.null(names(value)) || anyNA(value) ||
               is.numeric(value) && any(!is.finite(value)) ||
               is.character(value) && any(nchar(value, type = "bytes") > 16000L)) {
      stop("Expected finite plain JSON values in ", label, call. = FALSE)
    }
    invisible(NULL)
  }
  if (!is.list(x) || is.null(names(x)) || !length(x))
    stop(label, " must be a nonempty JSON object", call. = FALSE)
  walk(x)
  if (nchar(strict_json(x), type = "bytes") > 262144L)
    stop(label, " exceeds the JSON byte limit", call. = FALSE)
  x
}

strict_cttir_composition <- function(composition, lock) {
  strict_names(composition, c("schema_version", "study", "recipe", "runtime"), "cttiR composition")
  if (!identical(composition$schema_version, 1L) && !identical(composition$schema_version, 1))
    stop("Unknown cttiR composition version", call. = FALSE)
  study <- strict_cttir_json(composition$study, "study")
  recipe <- strict_cttir_json(composition$recipe, "recipe")
  runtime <- composition$runtime
  strict_names(runtime, c("package", "version", "writer", "files"), "cttiR runtime")
  if (!identical(runtime$package, "cttiR") || !identical(runtime$writer, "study_stage") ||
      !is.character(runtime$version) || length(runtime$version) != 1L || is.na(runtime$version) ||
      is.null(lock$Packages$cttiR) || !identical(runtime$version, lock$Packages$cttiR$Version))
    stop("The fixed cttiR study_stage writer must match the dependency lock", call. = FALSE)
  if (!is.list(runtime$files) || !length(runtime$files) || length(runtime$files) > 10000L ||
      any(!vapply(runtime$files, function(x) is.character(x) && length(x) == 1L && !is.na(x), logical(1))))
    stop("The cttiR runtime requires a complete named file digest object", call. = FALSE)
  hashes <- strict_review_hashes(runtime$files)
  if (!all(c("DESCRIPTION", "NAMESPACE") %in% names(hashes)))
    stop("The cttiR runtime inventory must include DESCRIPTION and NAMESPACE", call. = FALSE)
  runtime$files <- as.list(hashes[sort(names(hashes))])
  list(schema_version = 1L, study = study, recipe = recipe, runtime = runtime)
}

# This self-contained function is also embedded in the canonical runtime helper.
# It reads installed package files without loading or evaluating their namespace.
strict_cttir_runtime_status <- function(runtime, package_path = NULL) {
  tryCatch({
    if (is.null(package_path)) package_path <- find.package("cttiR", quiet = TRUE)
    if (length(package_path) != 1L || !dir.exists(package_path)) stop("Pinned cttiR installation unavailable")
    root <- as.character(fs::path_abs(package_path))
    parent <- root
    repeat {
      link <- Sys.readlink(parent)
      if (!is.na(link) && nzchar(link)) stop("cttiR runtime ancestors must not be symbolic links")
      next_parent <- dirname(parent)
      if (identical(next_parent, parent)) break
      parent <- next_parent
    }
    visit <- function(path) {
      kind <- as.character(fs::file_info(path, follow = FALSE, fail = FALSE)$type)
      if (length(kind) != 1L || is.na(kind) || !kind %in% c("file", "directory"))
        stop("cttiR runtime requires regular files and directories without links")
      if (kind == "file") return(path)
      unlist(lapply(list.files(path, all.files = TRUE, no.. = TRUE, full.names = TRUE), visit), use.names = FALSE)
    }
    paths <- visit(root)
    relative <- substring(paths, nchar(root) + 2L)
    if (!setequal(relative, names(runtime$files))) stop("cttiR runtime file inventory differs")
    for (i in seq_along(paths)) {
      if (!identical(digest::digest(file = paths[[i]], algo = "sha256"), runtime$files[[relative[[i]]]]))
        stop("cttiR runtime file hash differs: ", relative[[i]])
    }
    description <- read.dcf(file.path(root, "DESCRIPTION"))
    if (nrow(description) != 1L || !all(c("Package", "Version") %in% colnames(description)) ||
        !identical(unname(description[1L, "Package"]), "cttiR") ||
        !identical(unname(description[1L, "Version"]), runtime$version)) stop("cttiR runtime version differs")
    namespace <- parse(file = file.path(root, "NAMESPACE"), keep.source = FALSE)
    exported <- any(vapply(as.list(namespace), function(x) {
      is.call(x) && identical(x[[1L]], as.name("export")) && any(vapply(as.list(x)[-1L], function(y) {
        (is.symbol(y) || is.character(y) && length(y) == 1L) && identical(as.character(y), "study_stage")
      }, logical(1)))
    }, logical(1)))
    if (!exported) stop("cttiR runtime does not explicitly export study_stage")
    if ("cttiR" %in% loadedNamespaces() &&
        !identical(normalizePath(getNamespaceInfo("cttiR", "path")), normalizePath(root)))
      stop("Loaded cttiR namespace belongs to another library")
    list(current = TRUE, reason = "Installed cttiR files match the external runtime declaration")
  }, error = function(e) list(current = FALSE, reason = conditionMessage(e)))
}

strict_cttir_runtime_installed <- function(runtime) strict_cttir_runtime_status(runtime)

strict_cttir_runtime_text <- function(expected_controls) {
  check <- paste(deparse(strict_cttir_runtime_status, width.cutoff = 100L), collapse = "\n")
  paste0("\n# Composed profile: verify the fixed installed writer before each report stage.\n",
    ".strict_cttir_runtime_status <- ", check, "\n",
    ".strict_cttir_expected_controls <- ", paste(deparse(expected_controls, width.cutoff = 100L), collapse = "\n"), "\n",
    strict_template("cttir-runtime.R"))
}

strict_cttir_stage_rmd <- function(stage, profile, author) {
  text <- strict_stage_rmd(stage, profile, author)
  if (!stage$applicable) return(text)
  text <- sub("Status: Planned. This scaffold contains no completed analysis.",
    "Status: Recorded results require current analysis and provenance checks.", text, fixed = TRUE)
  text <- sub("Record the planned question, inputs, decisions and assumptions.",
    "The fixed study writer presents the bound study plan and recorded results below.", text, fixed = TRUE)
  text <- sub("```{r analysis}", "```{r analysis, results='asis'}", text, fixed = TRUE)
  sub("Report results only after the reviewed analysis has run.",
    "The report verifies recorded outputs; rendering does not recompute or scientifically approve the study.", text, fixed = TRUE)
}
