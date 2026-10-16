# Versioned runtime helper. Dependencies must already be installed.
.strict_runtime_files <- function(root) {
  visit <- function(path) {
    link <- Sys.readlink(path)
    if (!is.na(link) && nzchar(link)) stop("Symbolic links are not allowed: ", path)
    if (!dir.exists(path)) return(path)
    children <- list.files(path, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    unlist(lapply(children, visit), use.names = FALSE)
  }
  if (!dir.exists(root)) return(character())
  sort(visit(root))
}
.strict_runtime_hashes <- function(paths) {
  stats::setNames(lapply(paths, function(path) digest::digest(file = path, algo = "sha256")), paths)
}
.strict_output_files <- function() {
  paths <- .strict_runtime_files("output")
  paths[!startsWith(paths, "output/provenance/") & !grepl("/README[.]md$", paths)]
}
.strict_output_times <- function(paths) {
  stats::setNames(as.numeric(file.info(paths)$mtime), paths)
}
strict_stage_begin <- function(stage, seed) {
  declaration <- jsonlite::read_json("reflow-profile.json", simplifyVector = FALSE)
  profile <- declaration$profile
  if (any(vapply(profile[c("data_access", "jurisdiction", "legal_basis")], function(x)
    !is.character(x) || length(x) != 1L || !nzchar(x) || identical(x, "unknown"), logical(1))))
    stop("Declare data access, jurisdiction and legal basis before stage execution")
  lock <- jsonlite::read_json("renv.lock", simplifyVector = FALSE)
  if (!identical(as.character(getRversion()), lock$R$Version)) stop("R version differs from renv.lock")
  for (name in names(lock$Packages)) {
    if (!requireNamespace(name, quietly = TRUE) ||
        !identical(as.character(utils::packageVersion(name)), lock$Packages[[name]]$Version))
      stop("Install or restore the reviewed dependency lock explicitly before rendering: ", name)
    if (identical(lock$Packages[[name]]$Source, "GitHub") &&
        !identical(utils::packageDescription(name)$RemoteSha, lock$Packages[[name]]$RemoteSha))
      stop("Installed package revision differs from renv.lock: ", name)
  }
  inputs <- .strict_runtime_files("data/raw")
  raw_paths <- c(inputs, list.dirs("data/raw", recursive = TRUE, full.names = TRUE))
  if (length(raw_paths) && any(bitwAnd(as.integer(file.info(raw_paths)$mode), 146L) != 0L))
    stop("Raw input files must be read-only")
  set.seed(seed)
  list(stage = stage, seed = seed, started_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
       inputs = .strict_runtime_hashes(inputs), outputs_before = .strict_runtime_hashes(.strict_output_files()),
       output_times_before = .strict_output_times(.strict_output_files()))
}
strict_stage_finish <- function(state, analysis_file, code_file) {
  current <- .strict_runtime_hashes(.strict_runtime_files("data/raw"))
  if (!identical(state$inputs, current)) stop("Raw inputs changed during rendering")
  output_hashes <- .strict_runtime_hashes(.strict_output_files())
  output_times <- .strict_output_times(names(output_hashes))
  # Unchanged artifacts from another stage cannot establish this stage's output.
  output_hashes <- output_hashes[!vapply(names(output_hashes), function(path)
    identical(output_hashes[[path]], state$outputs_before[[path]]) &&
      identical(unname(output_times[path]), unname(state$output_times_before[path])), logical(1))]
  receipt <- list(schema_version = 1L, stage = state$stage, seed = state$seed,
                  started_at = state$started_at, finished_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
                  execution_kind = "rendered; completion requires external reviewed code and output checks",
                  inputs = state$inputs, outputs = output_hashes,
                  code = .strict_runtime_hashes(c(analysis_file, code_file, "code/strict-setup.R", "renv.lock", "reflow-profile.json")),
                  R_version = as.character(getRversion()), session = utils::capture.output(utils::sessionInfo()))
  dir.create("output/provenance", showWarnings = FALSE, recursive = TRUE)
  path <- file.path("output/provenance", paste0(state$stage, ".json"))
  jsonlite::write_json(receipt, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(receipt)
}
