#' Run a declared artifact with an optional Linux guardian
#'
#' These additive entry points leave the synchronous artifact APIs unchanged.
#' They require Linux, processx >= 3.9.0, ps >= 1.9.3, /proc and explicit
#' executable/runtime file pins. They are not a sandbox: trusted same-UID
#' descendants must preserve their ownership environment. Guardian death is
#' an unresolved failure requiring external cleanup, never automatic retry.
#'
#' Address space is limited per process, not across the tree. Thread variables
#' are requests, not CPU quotas. Priority is absolute and cannot be elevated.
#' The first backend supports explicit finite deadlines of 1 to 86400 seconds.
#' The guardian must report readiness within the deadline, but no less than 5
#' and no more than 60 seconds; a guardian that misses this has started no
#' worker and is stopped. Cleanup can take
#' up to two additional five-second intervals. Unknown ownership retains locks.
#' Runtime pins cover declared files, not an inferred complete ELF closure.
#' Parent OPENBLAS_NUM_THREADS, OMP_NUM_THREADS and MKL_NUM_THREADS must
#' already equal the requested thread count. Child locale, RNG kind and the
#' full captured numerical environment must match before writer dispatch.
#' In-process parent RNG/locale changes are not silently replayed. Use an
#' explicit artifact seed for stochastic writers; RNG state is not copied.
#'
#' @param spec A [reflow_artifact_spec()].
#' @param directory Artifact run directory, new for run and existing for resume.
#' @param supervisor_directory A fresh, separate supervisor directory.
#' @param resources Exact named list with `timeout_seconds`,
#'   `address_space_bytes`, `nice`, `threads`, `temp_directory`,
#'   `cache_directory`, `rscript`, `prlimit`, and `nice_command`.
#' @param runtime_files Named SHA-256 character vector of canonical regular
#'   files. Must include all three executables, installed backend scripts and
#'   every file of the selected backend and declared writer dependency closure.
#' @param cancel Function returning one nonmissing logical value. TRUE or an
#'   error requests cancellation; errors remain failures after cleanup.
#' @param reconciled_attempt,reconciliation Explicit retry reconciliation as
#'   in [reflow_artifact_resume()]. No absence of subprocesses is inferred.
#' @return Verified artifact descriptor, only after successful guardian cleanup
#'   and a locked READY-only revalidation that cannot invoke a writer.
#' @export
reflow_artifact_run_bounded <- function(spec, directory, supervisor_directory,
                                        resources, runtime_files,
                                        cancel = function() FALSE) {
  rfb_run(
    spec, directory, supervisor_directory, resources, runtime_files,
    cancel, FALSE, NULL, NULL
  )
}

#' @rdname reflow_artifact_run_bounded
#' @export
reflow_artifact_resume_bounded <- function(spec, directory, supervisor_directory,
                                           resources, runtime_files,
                                           cancel = function() FALSE,
                                           reconciled_attempt = NULL,
                                           reconciliation = NULL) {
  rfb_run(
    spec, directory, supervisor_directory, resources, runtime_files,
    cancel, TRUE, reconciled_attempt, reconciliation
  )
}

rfb_capability <- function() {
  if (!identical(Sys.info()[["sysname"]], "Linux") ||
    !requireNamespace("processx", quietly = TRUE) ||
    !requireNamespace("ps", quietly = TRUE) ||
    utils::packageVersion("processx") < "3.9.0" ||
    utils::packageVersion("ps") < "1.9.3" || !ps::ps_is_supported()) {
    stop("Optional Linux bounded execution capability unavailable.")
  }
}

rfb_resources <- function(x) {
  keys <- c(
    "timeout_seconds", "address_space_bytes", "nice", "threads",
    "temp_directory", "cache_directory", "rscript", "prlimit",
    "nice_command"
  )
  if (!is.list(x) || is.object(x) || !is.null(dim(x)) ||
    !identical(sort(names(x)), sort(keys))) {
    stop("Exact resource fields required.")
  }
  num <- function(z, lo, hi, integral = FALSE) {
    typeof(z) %in% c("integer", "double") && !is.object(z) &&
      is.null(dim(z)) && length(z) == 1L && is.finite(z) &&
      z >= lo && z <= hi && (!integral || z == floor(z))
  }
  if (!num(x$timeout_seconds, 1, 86400) ||
    !num(x$address_space_bytes, 1, 2^53, TRUE) ||
    !num(x$nice, 0, 19, TRUE) || !num(x$threads, 1, 2, TRUE)) {
    stop("Invalid bounded resource limits.")
  }
  x
}

rfb_ready_only <- function(spec, directory, expected_definition) {
  lock <- file.path(directory, ".lock")
  if (!dir.create(lock, showWarnings = FALSE)) stop("Artifact verification locked.")
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  current <- rfa_signature(spec)
  saved <- rfa_read(file.path(directory, "definition.rds"))
  if (!identical(current, expected_definition) ||
    !identical(saved, expected_definition)) {
    stop("Artifact definition changed.")
  }
  ready <- rfa_read(file.path(directory, "READY.rds"))
  result <- rfa_descriptor(spec, directory, ready, saved)
  if (!identical(rfa_signature(spec), saved)) stop("Artifact changed during verification.")
  result
}

# Closes one end of a guardian channel. Errors are ignored: the guardian may
# have closed its end already.
rfb_close_channel <- function(con) {
  try(processx::processx_conn_close(con), silent = TRUE)
}

rfb_run <- function(spec, directory, supervisor_directory, resources,
                    runtime_files, cancel, resume, reconciled_attempt,
                    reconciliation, expected_definition = NULL) {
  rfb_capability()
  if (!is.function(cancel)) stop("Cancellation callback required.")
  current_definition <- rfa_signature(spec)
  if (is.null(expected_definition)) expected_definition <- current_definition
  if (!identical(current_definition, expected_definition)) {
    stop("Captured artifact definition changed before bounded preflight.")
  }
  preflight <- rfb_preflight(spec, resources, runtime_files)
  resources <- preflight$resources
  files <- preflight$files
  packages <- preflight$packages
  package_path <- preflight$package_path
  e <- preflight$environment
  verify <- function() {
    if (!identical(e$rfx_hashes(names(runtime_files)), runtime_files)) stop("Runtime changed.")
  }
  e$rfx_path(directory, exists = resume, directory = resume)
  e$rfx_path(supervisor_directory, exists = FALSE)
  for (name in c("temp_directory", "cache_directory")) {
    e$rfx_path(resources[[name]], directory = TRUE)
  }
  for (name in c("rscript", "prlimit", "nice_command")) {
    e$rfx_path(resources[[name]])
    if (file.access(resources[[name]], 1) != 0) stop("Executable unavailable.")
  }
  e$rfx_nice_plan(ps::ps_get_nice(ps::ps_handle()), resources$nice)
  paths <- c(directory, supervisor_directory)
  if (identical(paths[1], paths[2]) || rfa_within(paths[1], paths[2]) ||
    rfa_within(paths[2], paths[1])) {
    stop("Artifact and supervisor overlap.")
  }
  if (file.exists(supervisor_directory) || (!resume && file.exists(directory))) {
    stop("Fresh destination required.")
  }
  # Validate the actual artifact declaration before creating a supervisor.
  if (!e$rb_plain(spec)) stop("Data-only artifact declaration required.")
  if (!identical(rfa_signature(spec), expected_definition)) {
    stop("Artifact changed during preflight.")
  }
  rf_walk(spec$args, function(x) {
    if (identical(supervisor_directory, rfa_canonical(x$path)) ||
      (rfa_directory(x$path) && rfa_within(
        supervisor_directory,
        rfa_canonical(x$path)
      ))) {
      stop("Supervisor overlaps tracked input.")
    }
    x
  })
  cancelled <- function() {
    z <- cancel()
    if (!is.logical(z) || is.object(z) || !is.null(dim(z)) ||
      length(z) != 1L || is.na(z)) {
      stop("Invalid cancellation result.")
    }
    z
  }
  if (cancelled()) stop("Cancelled before launch.")
  if (any(grepl(.Platform$path.sep, .libPaths(),
    fixed = TRUE))) stop("Ambiguous library path separator.")
  declaration <- list(
    mode = "worker", libraries = .libPaths(), package_paths = packages, executor_script = files[1],
    command = resources$rscript,
    args = c("--vanilla", files[5], file.path(supervisor_directory, "request.rds")),
    wd = dirname(directory), runtime_sha256 = runtime_files,
    all_source_sha256 = runtime_files,
    temp_directory = resources$temp_directory,
    cache_directory = resources$cache_directory,
    deadline_seconds = resources$timeout_seconds,
    address_space_bytes = resources$address_space_bytes,
    nice = resources$nice, threads = resources$threads,
    environment = c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)),
      prlimit = resources$prlimit,
    nice_command = resources$nice_command
  )
  request <- list(
    schema = 1L, spec = spec, expected_definition = expected_definition,
    directory = directory,
    resume = resume, reconciled_attempt = reconciled_attempt,
    reconciliation = reconciliation, libraries = .libPaths(),
    package_path = package_path, package_paths = packages, runtime_sha256 = runtime_files,
    runtime_observation = list(locale = Sys.getlocale(), rng_kind = RNGkind()),
    channel_script = files[2]
  )
  e$rb_request(request)
  declaration$request <- request
  client <- e$rg_client_start(
    declaration, supervisor_directory, files[4],
    resources$rscript, files[2]
  )
  finished <- FALSE
  on.exit(
    {
      # Closing the control channel requests guardian-owned cleanup. Never kill
      # the guardian here: client death and interrupts must leave it time to act.
      rfb_close_channel(client$channel$write)
      rfb_close_channel(client$channel$read)
    },
    add = TRUE
  )
  error <- NULL
  start_sent <- FALSE
  tryCatch(
    {
      e$rg_client_wait(client, "READY",
        seconds = rfb_handshake_seconds(resources$timeout_seconds)
      )
      # From here on the guardian may own a worker and must clean up itself.
      start_sent <- TRUE
      e$rg_send(client$channel, "START", proc.time()[["elapsed"]] + 2)
      end <- proc.time()[["elapsed"]] + resources$timeout_seconds + 12
      sent <- FALSE
      while (client$process$is_alive()) {
        if (!sent && cancelled()) {
          e$rg_send(client$channel, "CANCEL", proc.time()[["elapsed"]] + 2)
          sent <- TRUE
        }
        if (proc.time()[["elapsed"]] >= end) stop("Guardian cleanup unresolved.")
        e$rg_receive(client$channel, 50L)
      }
      finished <- TRUE
    },
    error = function(x) error <<- conditionMessage(x),
    interrupt = function(x) error <<- "Client interrupted"
  )
  if (!is.null(error) || !finished) {
    rfb_close_channel(client$channel$write)
    cleanup_end <- proc.time()[["elapsed"]] + 12
    while (client$process$is_alive() && proc.time()[["elapsed"]] < cleanup_end) {
      Sys.sleep(0.05)
    }
    # Before START the guardian has no worker, so one that missed the
    # handshake and is still running is stopped instead of left behind.
    if (!start_sent && client$process$is_alive()) {
      try(client$process$kill_tree(), silent = TRUE)
      try(client$process$kill(), silent = TRUE)
    }
    diagnostic <- list(
      status = "CLIENT_FAILED", error = error,
      guardian_alive = client$process$is_alive()
    )
    e$rfx_new_rds(diagnostic, file.path(supervisor_directory, "client-failure.rds"))
    stop(error)
  }
  verify()
  result <- rfa_read(file.path(supervisor_directory, "guardian-result.rds"))
  if (!identical(result$status, "EXECUTION_SUCCEEDED_UNVERIFIED") ||
    !isTRUE(result$client_alive) || !isTRUE(result$worker_started) ||
    !is.null(result$execution$cleanup_error) ||
    !is.null(result$execution$post_pin_error) ||
    length(result$execution$owned_observation$owned) ||
    length(result$execution$owned_observation$uncertain)) {
    stop("Bounded execution failed; preserve supervisor evidence and reconcile.")
  }
  answer <- rfb_ready_only(spec, directory, expected_definition)
  verify()
  e$rfx_new_rds(
    list(status = "ARTIFACT_VERIFIED", descriptor = answer),
    file.path(supervisor_directory, "artifact-verified.rds")
  )
  answer
}

# Seconds the guardian has to report READY. Hashing the pinned closure takes
# longer on a busy machine, so the wait grows with the declared budget.
rfb_handshake_seconds <- function(timeout_seconds) {
  max(5, min(timeout_seconds, 60))
}

rfb_packages <- function(roots) {
  paths <- character()
  pending <- roots
  while (length(pending)) {
    name <- pending[[1]]
    pending <- pending[-1]
    if (name %in% names(paths)) next
    path <- find.package(name)
    if (isNamespaceLoaded(name) &&
      !identical(if (identical(name, "base")) {
        file.path(R.home("library"), "base")
      } else {
        getNamespaceInfo(asNamespace(name), "path")
      }, path)) {
      stop("Loaded namespace differs from selected package.")
    }
    paths[[name]] <- path
    description <- read.dcf(file.path(path, "DESCRIPTION"))
    fields <- intersect(c("Depends", "Imports"), colnames(description))
    text <- paste(description[1, fields], collapse = ",")
    dependencies <- trimws(sub("[[:space:]]*\\(.*", "", strsplit(text, ",", fixed = TRUE)[[1]]))
    pending <- unique(c(pending, setdiff(dependencies[nzchar(dependencies)], "R")))
  }
  paths
}

rfb_thread_preflight <- function(threads) {
  keys <- c("OPENBLAS_NUM_THREADS", "OMP_NUM_THREADS", "MKL_NUM_THREADS")
  observed <- Sys.getenv(keys, unset = NA_character_)
  if (!identical(unname(observed), rep(as.character(threads), length(keys)))) {
    stop("Parent numerical thread environment differs from requested child settings.")
  }
  invisible(TRUE)
}

rfb_preflight <- function(spec, resources, runtime_files) {
  rfb_capability()
  resources <- rfb_resources(resources)
  rfb_thread_preflight(resources$threads)
  scripts <- system.file("bounded", package = "reflowR", mustWork = TRUE)
  files <- file.path(scripts, c(
    "executor.R", "channel.R", "client.R",
    "guardian.R", "worker.R", "schema.R"
  ))
  package_path <- find.package("reflowR")
  packages <- rfb_packages(unique(c(
    "reflowR", "processx", "ps", "digest",
    spec$package, spec$packages
  )))
  package_files <- unlist(lapply(packages, list.files,
    recursive = TRUE,
    full.names = TRUE, all.files = TRUE
  ), use.names = FALSE)
  expected <- c(files, package_files, resources$rscript, resources$prlimit, resources$nice_command)
  if (!is.character(runtime_files) || is.object(runtime_files) ||
    !is.null(dim(runtime_files)) || is.null(names(runtime_files)) ||
    anyNA(runtime_files) || anyNA(names(runtime_files)) ||
    anyDuplicated(names(runtime_files)) ||
    !all(grepl("^[0-9a-f]{64}$", runtime_files)) ||
    !all(expected %in% names(runtime_files))) {
    stop("Complete declared runtime pins required.")
  }
  for (file in names(runtime_files)) {
    rfa_no_links(file)
    if (!rfa_regular(file) || !identical(rfa_canonical(file), file) ||
      !identical(
        digest::digest(file = file, algo = "sha256", serialize = FALSE),
        runtime_files[[file]]
      )) {
      stop("Runtime pin mismatch.")
    }
  }
  e <- new.env(parent = baseenv())
  for (file in files[c(1:3, 6)]) sys.source(file, envir = e)
  for (name in c("temp_directory", "cache_directory")) {
    e$rfx_path(resources[[name]], directory = TRUE)
  }
  for (name in c("rscript", "prlimit", "nice_command")) {
    e$rfx_path(resources[[name]])
    if (file.access(resources[[name]], 1) != 0) stop("Executable unavailable.")
  }
  e$rfx_nice_plan(ps::ps_get_nice(ps::ps_handle()), resources$nice)
  if (!e$rb_plain(spec)) stop("Data-only artifact declaration required.")
  list(resources = resources, files = files, package_path = package_path,
       packages = packages, environment = e)
}
