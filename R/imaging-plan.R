#' Declare an imaging workflow stage
#'
#' Stages call installed exported functions; they do not evaluate script text.
#' Scientific methods and validation remain in the called packages. Calls must
#' return serializable values and must not mutate their inputs or write external
#' outputs. This executor is not a sandbox: use trusted package functions only.
#' @param id Stage identifier containing letters, digits, underscores; unique
#'   within a plan even when letter case is ignored.
#' @param package Installed package name.
#' @param fun Exported function name.
#' @param args Named list of arguments, including explicit references if needed.
#' @param seed Optional integer seed for stochastic calls. Calls consuming the
#'   ambient random stream without an explicit stage seed fail.
#' @return A stage specification.
#' @examples
#' stage <- reflow_imaging_stage("summary", "base", "summary",
#'                               args = list(object = c(2, 4, 4, 5)))
#' stage$id
#' # Stochastic calls need an explicit seed
#' noisy <- reflow_imaging_stage("noise", "stats", "rnorm", list(n = 3), seed = 1L)
#' noisy$seed
#' @export
reflow_imaging_stage <- function(id, package, fun, args = list(), seed = NULL) {
  for (x in list(id, package, fun)) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
      stop("Stage identifiers must be nonempty strings.", call. = FALSE)
    }
  }
  if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", id)) stop("Invalid stage id.")
  bad_names <- is.null(names(args)) || anyNA(names(args)) ||
    any(!nzchar(names(args))) ||
    anyDuplicated(names(args))
  if (!is.list(args) || (length(args) && bad_names)) {
    stop("args must be a uniquely named list.")
  }
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
          !is.finite(seed) || seed < 0 || seed > .Machine$integer.max ||
          seed != as.integer(seed)) {
      stop("seed must be a nonnegative integer or NULL.")
    }
  }
  structure(list(id = id, package = package, fun = fun, args = args, seed = seed),
    class = "reflow_imaging_stage"
  )
}

#' Reference an upstream result or an input file
#' @param stage Identifier of an earlier stage.
#' @param select Character vector of nested list components to extract.
#' @return An explicit dependency reference.
#' @examples
#' # Pass the value of an earlier stage, or one component of it
#' reflow_imaging_ref("fit")
#' reflow_imaging_ref("fit", select = "coefficients")
#'
#' # Track an input file: its contents are hashed into the run definition
#' path <- tempfile(fileext = ".rds")
#' saveRDS(c(2, 4, 4, 5), path)
#' input <- reflow_imaging_input(path, read = TRUE)
#' input$read
#' unlink(path)
#' @export
reflow_imaging_ref <- function(stage, select = character()) {
  if (!is.character(stage) || length(stage) != 1L || is.na(stage) ||
        !is.character(select) || anyNA(select) || any(!nzchar(select))) {
    stop("Invalid reference.")
  }
  structure(list(stage = stage, select = select), class = "reflow_imaging_ref")
}

#' @rdname reflow_imaging_ref
#' @param path Existing input file or directory. File contents and directory
#'   layout, including empty directories, are fingerprinted. Imaging inputs may
#'   follow symbolic links; cached file hashes use the final target's metadata.
#' @param read Read an RDS input as an object; otherwise pass its absolute path.
#' @export
reflow_imaging_input <- function(path, read = FALSE) {
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
        !is.logical(read) || length(read) != 1L || is.na(read)) {
    stop("Invalid input.")
  }
  if (read && dir.exists(path)) stop("An RDS input must be a file.")
  structure(list(path = normalizePath(path, mustWork = TRUE), read = read),
    class = "reflow_imaging_input"
  )
}

#' Assemble a sequential imaging dependency graph
#'
#' Stages must be in dependency order. Forward references and cycles fail.
#' File inputs must use [reflow_imaging_input()] so content changes invalidate a
#' resume. Plain character arguments are configuration, not tracked file inputs.
#' @param ... Stage specifications.
#' @param packages Additional installed packages to fingerprint, for example
#'   optional backends used by a stage. Declare these explicitly.
#' @return A versioned plan; no stage is executed.
#' @examples
#' plan <- reflow_imaging_plan(
#'   reflow_imaging_stage("values", "base", "identity",
#'                        list(x = c(2, 4, 4, 5))),
#'   reflow_imaging_stage("mean", "base", "mean",
#'                        list(x = reflow_imaging_ref("values")))
#' )
#' vapply(plan$stages, `[[`, character(1), "id")
#' @export
reflow_imaging_plan <- function(..., packages = character()) {
  if (!is.character(packages) || anyNA(packages) || any(!nzchar(packages))) {
    stop("packages must contain package names.")
  }
  stages <- list(...)
  valid <- vapply(stages, inherits, logical(1), "reflow_imaging_stage")
  if (!length(stages) || !all(valid)) {
    stop("Supply at least one stage.")
  }
  ids <- vapply(stages, `[[`, character(1), "id")
  # Ids name receipt files, so they must also differ in more than case.
  if (anyDuplicated(tolower(ids))) stop("Duplicate stage ids.")
  seen <- character()
  for (s in stages) {
    do.call(reflow_imaging_stage, unclass(s))
    rf_walk(s$args, function(x) {
      if (inherits(x, "reflow_imaging_ref") && !x$stage %in% seen) {
        stop("Reference must identify an earlier stage: ", x$stage)
      }
      x
    })
    seen <- c(seen, s$id)
  }
  structure(
    list(schema = "reflow_imaging_1", stages = stages,
         packages = sort(unique(packages))),
    class = "reflow_imaging_plan"
  )
}

rf_walk <- function(x, f) {
  if (inherits(x, "reflow_imaging_ref") || inherits(x, "reflow_imaging_input")) {
    return(f(x))
  }
  if (isS4(x) || !(is.null(x) || is.atomic(x) || is.list(x))) {
    stop("Non-atomic and non-list objects are unsupported.")
  }
  if (is.list(x)) x[] <- lapply(x, rf_walk, f = f)
  x
}

rf_hash <- function(x) {
  digest::digest(serialize(x, NULL, version = 2),
    algo = "sha256", serialize = FALSE
  )
}
rf_file_hash <- function(path) rf_file_hashes(path)

# SHA-256 of file contents, one call for many files. tools::sha256sum()
# (R >= 4.5.0) gives the same values as digest and is faster.
rf_file_hashes <- function(paths) {
  paths <- as.character(paths)
  if (!length(paths)) return(character())
  hashes <- if (getRversion() >= "4.5.0") {
    unname(getExportedValue("tools", "sha256sum")(paths))
  } else {
    # digest stops with its own message on unreadable paths; NA keeps the error below.
    vapply(paths, function(p) tryCatch(digest::digest(file = p, algo = "sha256"),
                                       error = function(e) NA_character_),
      character(1), USE.NAMES = FALSE)
  }
  if (anyNA(hashes)) {
    stop("Cannot read file for hashing: ", paths[is.na(hashes)][1L])
  }
  hashes
}

rf_package_cache <- new.env(parent = emptyenv())

# Hashes of installed package files. With
# options(reflowR.package_hash_cache = TRUE) a hash is reused within the R
# process while the file keeps its device, inode, size, modification and
# change times; linked files use the target's metadata, matching the bytes
# that hashing reads. Otherwise every call reads every file.
rf_package_hashes <- function(paths) rf_stamped_hashes(paths, "reflowR.package_hash_cache")

# Hashes of tracked input files, reused in the same way with
# options(reflowR.input_hash_cache = TRUE). Large raw inputs referenced by
# many stages are then read once per process.
rf_input_hashes <- function(paths) rf_stamped_hashes(paths, "reflowR.input_hash_cache")

rf_stamped_hashes <- function(paths, option) {
  paths <- as.character(paths)
  if (!length(paths) || !isTRUE(getOption(option, FALSE))) {
    return(rf_file_hashes(paths))
  }
  info <- fs::file_info(paths, follow = TRUE, fail = FALSE)
  stamp <- function(x) sprintf("%.9f", as.numeric(x))
  keys <- paste(paths, info$device_id, info$inode, info$size,
    stamp(info$modification_time), stamp(info$change_time),
    sep = "\r"
  )
  known <- vapply(keys, exists, logical(1),
    envir = rf_package_cache, inherits = FALSE, USE.NAMES = FALSE
  )
  if (any(!known) || anyNA(info$inode)) {
    todo <- !known | is.na(info$inode)
    fresh <- rf_file_hashes(paths[todo])
    for (i in seq_along(fresh)) assign(keys[todo][i], fresh[i], envir = rf_package_cache)
  }
  vapply(keys, get, character(1),
    envir = rf_package_cache, inherits = FALSE, USE.NAMES = FALSE
  )
}

rf_tree <- function(path) {
  if (!file.exists(path)) stop("Missing tracked input: ", path)
  type <- fs::file_info(path, follow = TRUE, fail = FALSE)$type
  if (identical(as.character(type), "file")) {
    return(stats::setNames(rf_input_hashes(path), basename(path)))
  }
  if (!identical(as.character(type), "directory")) {
    stop("Tracked inputs must be regular files or directories: ", path)
  }
  visit <- function(directory, ancestors = character()) {
    target <- as.character(fs::path_real(directory))
    if (target %in% ancestors) stop("Tracked directory link cycle: ", directory)
    entries <- as.character(fs::dir_ls(directory, all = TRUE, recurse = FALSE))
    types <- as.character(fs::file_info(entries, follow = TRUE, fail = FALSE)$type)
    invalid <- is.na(types) | !types %in% c("file", "directory")
    if (any(invalid)) {
      stop("Tracked inputs must be regular files or directories: ", entries[invalid][1])
    }
    directories <- entries[types == "directory"]
    nested <- lapply(directories, visit, ancestors = c(ancestors, target))
    list(
      directories = c(directories, unlist(lapply(nested, `[[`, "directories"),
                                          use.names = FALSE)),
      files = c(entries[types == "file"], unlist(lapply(nested, `[[`, "files"),
                                                use.names = FALSE))
    )
  }
  tree <- visit(path)
  files <- sort(tree$files)
  list(
    directories = substring(sort(tree$directories), nchar(path) + 2L),
    files = stats::setNames(rf_input_hashes(files),
                           substring(files, nchar(path) + 2L))
  )
}

# Runtime closure of packages: Depends and Imports, recursively, plus reflowR
# and digest. LinkingTo packages only supply headers at build time; what they
# contribute is in the compiled libs/ of the linking package, which is hashed.
rf_closure <- function(packages) {
  visited <- character()
  pending <- unique(c("reflowR", "digest", packages))
  while (length(pending)) {
    package <- pending[[1]]
    pending <- pending[-1]
    if (package %in% visited) next
    # find.package honours an already loaded namespace, unlike the first
    # installed.packages row when another library contains a newer version.
    path <- find.package(package, quiet = TRUE)
    if (!length(path)) stop("Missing packages: ", package)
    description <- read.dcf(file.path(path, "DESCRIPTION"))
    fields <- intersect(c("Depends", "Imports"), colnames(description))
    text <- paste(description[1, fields], collapse = ",")
    dependencies <- trimws(sub("[[:space:]]*\\(.*", "",
                              strsplit(text, ",", fixed = TRUE)[[1]]))
    visited <- c(visited, package)
    pending <- unique(c(pending, setdiff(dependencies[nzchar(dependencies)], "R")))
  }
  sort(visited)
}

rf_packages <- function(stages, extra = character()) {
  pkgs <- unique(c(extra, vapply(stages, `[[`, character(1), "package")))
  missing <- pkgs[!vapply(pkgs, function(p) length(find.package(p, quiet = TRUE)) > 0L,
                         logical(1))]
  if (length(missing)) {
    stop("Missing packages: ", paste(missing, collapse = ", "))
  }
  pkgs <- rf_closure(pkgs)
  stats::setNames(lapply(pkgs, function(p) {
    root <- find.package(p)
    files <- c(
      file.path(root, c("DESCRIPTION", "NAMESPACE")),
      list.files(file.path(root, "R"), full.names = TRUE),
      list.files(file.path(root, "libs"), recursive = TRUE, full.names = TRUE)
    )
    files <- sort(files[file.exists(files) & !dir.exists(files)])
    list(
      version = as.character(utils::packageVersion(p)),
      files = stats::setNames(
        rf_package_hashes(files),
        substring(files, nchar(root) + 2L)
      )
    )
  }), pkgs)
}

rf_signature <- function(plan) {
  if (!inherits(plan, "reflow_imaging_plan") ||
        !identical(plan$schema, "reflow_imaging_1")) {
    stop("Unsupported plan schema.")
  }
  do.call(reflow_imaging_plan, c(plan$stages, list(packages = plan$packages)))
  packages <- rf_packages(plan$stages, plan$packages)
  calls <- lapply(plan$stages, function(s) {
    fn <- getExportedValue(s$package, s$fun)
    if (!is.function(fn)) stop("Stage export is not a function: ", s$fun)
    rf_hash(list(
      deparse(formals(fn), width.cutoff = 500L),
      deparse(body(fn), width.cutoff = 500L)
    ))
  })
  inputs <- rf_walk(lapply(plan$stages, `[[`, "args"), function(x) {
    if (inherits(x, "reflow_imaging_input")) {
      return(list(input = x, hashes = rf_tree(x$path)))
    }
    x
  })
  numerical_environment <- Sys.getenv(c(
    "OMP_NUM_THREADS", "OMP_THREAD_LIMIT", "OMP_DYNAMIC",
    "OPENBLAS_NUM_THREADS", "GOTO_NUM_THREADS", "MKL_NUM_THREADS",
    "MKL_DYNAMIC", "MKL_CBWR", "VECLIB_MAXIMUM_THREADS",
    "BLIS_NUM_THREADS", "RCPP_PARALLEL_NUM_THREADS"
  ), unset = NA_character_)
  list(
    plan = plan, plan_hash = rf_hash(plan),
    numerical_environment = numerical_environment,
    inputs = inputs, input_hash = rf_hash(inputs),
    package_hash = rf_hash(packages), packages = packages, calls = calls,
    runtime_hash = rf_hash(list(
      R.version, Sys.info()[c("sysname", "release", "machine")],
      extSoftVersion(), Sys.getlocale(), RNGkind(), numerical_environment
    ))
  )
}
