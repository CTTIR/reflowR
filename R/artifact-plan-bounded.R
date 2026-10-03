#' Execute an artifact graph with a Linux guardian for each pending node
#'
#' These optional APIs preserve the synchronous graph APIs and scientific
#' definitions. Every stage has an explicit resource policy validated before
#' any writer. Completed nodes and prior-generation reuse use locked READY-only
#' verification, with no new supervisor or writer. Pending nodes run serially.
#'
#' The separate supervisor directory contains an immutable policy definition,
#' a required attempt journal, and retained per-attempt evidence. Resume refuses
#' policy changes, missing journals, unexplained directories and mismatched
#' execution signatures. An orphan READY is adopted only after a matching
#' successful guardian outcome and fresh READY verification. Client loss or
#' missing guardian evidence never authorizes adoption. Failed retries require
#' explicit artifact reconciliation and externally inspected guardian locks.
#'
#' Locks are acquired in graph, control, artifact/supervisor order. Owned graph
#' and control locks are released on ordinary R unwind, including errors. Stale
#' locks after process death are never stolen. Guardian death remains an
#' unqualified failure requiring external cleanup. Limits and trusted-child
#' assumptions are those of [reflow_artifact_run_bounded()]: per-process address
#' space, requested threads, Linux only, and 1 to 60 seconds per node. There is
#' no graph-wide time budget, parallel scheduler, selector or targets adapter.
#' @param plan A [reflow_artifact_plan()].
#' @param directory Fresh graph directory, or existing directory for resume.
#' @param supervisor_directory Separate fresh control root, or existing root
#'   for resume. Its parent must exist; it must not overlap tracked inputs,
#'   runtime files, graph, temporary or cache directories.
#' @param resources Plain named list in canonical stage order, with one exact
#'   resource list per stage as in [reflow_artifact_run_bounded()].
#' @param runtime_files Common named SHA-256 vector covering every stage's
#'   installed writer dependency closure and bounded backend.
#' @param previous Optional completed prior graph for verified in-place reuse.
#' @param cancel Cancellation callback, as in [reflow_artifact_run_bounded()].
#' @param reconciliations Explicit per-stage artifact retry reconciliations,
#'   as in [reflow_artifact_plan_resume()]. These never clear a guardian lock.
#' @return A verified graph descriptor. No scientific adoption is implied.
#' @export
reflow_artifact_plan_run_bounded <- function( # nolint: object_length_linter.
  plan, directory, supervisor_directory, resources, runtime_files,
  previous = NULL, cancel = function() FALSE
) {
  rfbg_execute(plan, directory, previous, TRUE, list(),
               supervisor_directory, resources, runtime_files, cancel)
}

#' @rdname reflow_artifact_plan_run_bounded
#' @export
reflow_artifact_plan_resume_bounded <- function( # nolint: object_length_linter.
  plan, directory, supervisor_directory, resources, runtime_files,
  cancel = function() FALSE, reconciliations = list()
) {
  rfbg_execute(plan, directory, NULL, FALSE, reconciliations,
               supervisor_directory, resources, runtime_files, cancel)
}

rfbg_cancel <- function(cancel) {
  if (!is.function(cancel)) stop("Cancellation callback required.")
  x <- cancel()
  if (!is.logical(x) || is.object(x) || !is.null(dim(x)) ||
      length(x) != 1L || is.na(x)) stop("Invalid cancellation result.")
  if (x) stop("Graph cancelled before dispatch.")
  invisible(TRUE)
}

rfbg_path <- function(path, exists = FALSE) {
  rfg_text(path)
  rfa_no_links(path)
  if (!startsWith(path, "/") || !rfa_directory(dirname(path)) ||
      !identical(path, file.path(rfa_canonical(dirname(path)), basename(path)))) {
    stop("Canonical graph/control path with existing parent required.")
  }
  if (exists && !rfa_directory(path)) stop("Control directory missing.")
  path
}

rfbg_disjoint <- function(a, b) {
  if (rfa_within(a, b) || rfa_within(b, a)) stop("Bounded graph paths overlap.")
  invisible(TRUE)
}

rfbg_preflight <- function(plan, directory, supervisor_directory, resources,
                           runtime_files, cancel, fresh) {
  rfbg_cancel(cancel)
  current <- rfg_definition(plan)
  ids <- names(plan$stages)
  if (!is.list(resources) || is.object(resources) || !is.null(dim(resources)) ||
      !identical(names(resources), ids)) stop("Exact ordered stage policy required.")
  directory <- rfbg_path(directory, !fresh)
  supervisor_directory <- rfbg_path(supervisor_directory, !fresh)
  rfbg_disjoint(directory, supervisor_directory)
  if (fresh && (file.exists(directory) || file.exists(supervisor_directory))) {
    stop("Fresh graph and control directories required.")
  }
  for (id in ids) {
    spec <- plan$stages[[id]]$declaration
    spec$args <- rfg_walk(spec$args, function(x) "deferred-artifact")
    # No process or output allocation: exact runtime/capability/package checks.
    checked <- rfb_preflight(spec, resources[[id]], runtime_files)
    resources[[id]] <- checked$resources
    for (path in c(names(runtime_files), resources[[id]]$temp_directory,
                   resources[[id]]$cache_directory)) {
      rfbg_disjoint(directory, path)
      rfbg_disjoint(supervisor_directory, path)
    }
    rf_walk(spec$args, function(x) {
      path <- rfa_canonical(x$path)
      rfbg_disjoint(directory, path)
      rfbg_disjoint(supervisor_directory, path)
      x
    })
  }
  if (!identical(rfg_definition(plan), current)) stop("Graph changed during policy preflight.")
  list(directory = supervisor_directory,
       definition = list(schema = "reflow_bounded_graph_control_1",
                         graph_directory = directory,
                         resources = resources, runtime_files = runtime_files,
                         libraries = .libPaths(),
                         runtime_observation = list(locale = Sys.getlocale(),
                                                    rng_kind = RNGkind()),
                         backend = "linux_guardian_1", graph_definition = NULL))
}

rfbg_open <- function(control, fresh) {
  path <- control$directory
  if (fresh) {
    if (file.exists(path) || !dir.create(path, showWarnings = FALSE)) {
      stop("Fresh control directory required.")
    }
    rf_write(control$definition, file.path(path, "definition.rds"))
    rf_write(list(), file.path(path, "attempts.rds"))
  } else if (!identical(rfg_read(file.path(path, "definition.rds")),
                        control$definition)) {
    stop("Bounded graph policy or definition changed.")
  }
  invisible(TRUE)
}

rfbg_control <- function(control) {
  path <- control$directory
  if (!identical(rfg_read(file.path(path, "definition.rds")), control$definition)) {
    stop("Control definition changed.")
  }
  records <- rfg_read(file.path(path, "attempts.rds"))
  if (!is.list(records) || is.object(records) || !is.null(names(records))) {
    stop("Malformed bounded attempt journal.")
  }
  ids <- names(control$definition$resources)
  dirs <- character()
  prior_stage <- 0L
  for (i in seq_along(records)) {
    x <- records[[i]]
    if (!is.list(x) || is.object(x) || !identical(names(x),
        c("stage", "signature", "dependencies", "supervisor", "resume",
          "retry", "client_identity", "client_time", "state", "proof", "descriptor")) ||
        !x$stage %in% ids || !x$state %in% c("INTENT", "VERIFIED", "JOURNALED") ||
        !identical(x$supervisor, file.path(path, sprintf("attempt-%06d", i))) ||
        !is.logical(x$resume) || length(x$resume) != 1L || is.na(x$resume)) {
      stop("Malformed bounded attempt record.")
    }
    stage_index <- match(x$stage, ids)
    if (stage_index < prior_stage) stop("Attempt journal stage order differs.")
    prior_stage <- stage_index
    if (x$state == "INTENT" && (!is.null(x$proof) || !is.null(x$descriptor))) {
      stop("Unverified attempt carries completion.")
    }
    if (x$state != "INTENT") {
      if (!is.character(x$proof) || !is.list(x$descriptor)) {
        stop("Missing bounded completion proof.")
      }
      rfbg_proof(control, x, expected = x$proof)
    }
    dirs <- c(dirs, basename(x$supervisor))
  }
  entries <- list.files(path, all.files = TRUE, no.. = TRUE)
  if (any(!entries %in% c(".lock", "definition.rds", "attempts.rds", dirs))) {
    stop("Unexpected bounded control inventory.")
  }
  for (d in intersect(dirs, entries)) {
    rfa_no_links(file.path(path, d))
    if (!rfa_directory(file.path(path, d))) stop("Attempt directory required.")
  }
  records
}

rfbg_proof <- function(control, attempt, expected = NULL) {
  path <- attempt$supervisor
  rfa_no_links(path)
  rfa_no_links(file.path(path, "guardian.lock"))
  if (file.exists(file.path(path, "guardian.lock"))) {
    stop("Guardian lock retained; external inspection required.")
  }
  failure <- file.path(path, "client-failure.rds")
  rfa_no_links(failure)
  if (file.exists(failure)) stop("Client failure forbids READY adoption.")
  request <- rfg_read(file.path(path, "request.rds"))
  resource <- control$definition$resources[[attempt$stage]]
  definition <- control$definition
  checked <- rfb_preflight(attempt$signature$spec, resource, definition$runtime_files)
  declaration <- rfg_read(file.path(path, "declaration.rds"))
  result <- rfg_read(file.path(path, "guardian-result.rds"))
  worker_lock <- file.path(path, "worker", "supervisor.lock")
  rfa_no_links(worker_lock)
  if (file.exists(worker_lock)) stop("Worker supervisor lock retained.")
  rfbg_causal_fields(control, attempt, checked, request, declaration, result,
    rf_file_hash(file.path(path, "request.rds")),
    rfg_read(file.path(path, "guardian-ready.rds")),
    rfg_read(file.path(path, "worker", "declaration.rds")),
    rfg_read(file.path(path, "worker", "result.rds")),
    rfg_read(file.path(path, "worker", "child.rds")))
  relative <- c("request.rds", "declaration.rds", "guardian-result.rds",
                "guardian-ready.rds", "worker/declaration.rds",
                "worker/result.rds", "worker/child.rds")
  files <- file.path(path, relative)
  proof <- stats::setNames(vapply(files, rf_file_hash, ""), relative)
  if (!is.null(expected) && !identical(proof, expected)) stop("Supervisor proof changed.")
  proof
}

rfbg_dispatch <- function(control, id, spec, node_path, signature, dependencies,
                          retry, cancel) {
  rfbg_cancel(cancel)
  records <- rfbg_control(control)
  indices <- which(vapply(records, function(x) identical(x$stage, id), logical(1)))
  if (length(indices)) {
    last <- records[[utils::tail(indices, 1L)]]
    if (!identical(last$signature, signature) || !identical(last$dependencies, dependencies)) {
      stop("Pending bounded signature or lineage changed.")
    }
    if (file.exists(file.path(node_path, "READY.rds"))) {
      proof <- rfbg_proof(control, last)
      descriptor <- rfb_ready_only(spec, node_path, signature)
      last$state <- "VERIFIED"
      last$proof <- proof
      last$descriptor <- descriptor
      records[[utils::tail(indices, 1L)]] <- last
      rf_write(records, file.path(control$directory, "attempts.rds"))
      return(descriptor)
    }
    # Retry is an explicit caller assertion, never inferred from a missing PID.
    if (is.null(retry)) stop("Explicit failed-stage reconciliation required.")
    rfa_no_links(file.path(last$supervisor, "guardian.lock"))
    if (file.exists(file.path(last$supervisor, "guardian.lock"))) {
      stop("Guardian lock retained; external inspection required.")
    }
  } else if (file.exists(node_path)) {
    stop("Local artifact has no bounded execution intent.")
  }
  supervisor <- file.path(control$directory, sprintf("attempt-%06d", length(records) + 1L))
  checked <- rfb_preflight(spec, control$definition$resources[[id]],
                           control$definition$runtime_files)
  client_identity <- checked$environment$rg_stable_identity(Sys.getpid())
  client_time <- ps::ps_create_time(ps::ps_handle())
  attempt <- list(stage = id, signature = signature, dependencies = dependencies,
                  supervisor = supervisor, resume = file.exists(node_path),
                  retry = retry, client_identity = client_identity,
                  client_time = client_time, state = "INTENT", proof = NULL,
                  descriptor = NULL)
  records[[length(records) + 1L]] <- attempt
  rf_write(records, file.path(control$directory, "attempts.rds"))
  descriptor <- rfb_run(spec, node_path, supervisor,
                        control$definition$resources[[id]], control$definition$runtime_files,
                        cancel, attempt$resume, retry$reconciled_attempt,
                        retry$reconciliation, expected_definition = signature)
  attempt$proof <- rfbg_proof(control, attempt)
  attempt$state <- "VERIFIED"
  attempt$descriptor <- descriptor
  records[[length(records)]] <- attempt
  rf_write(records, file.path(control$directory, "attempts.rds"))
  descriptor
}

rfbg_completed <- function(control, id, node, directory) {
  records <- rfbg_control(control)
  local <- identical(node$descriptor$directory, file.path(directory, id))
  indices <- which(vapply(records, function(x) identical(x$stage, id), logical(1)))
  if (!local) {
    if (length(indices)) stop("Reused node has unexplained local attempts.")
    return(invisible(TRUE))
  }
  if (!length(indices)) stop("Completed node lacks bounded provenance.")
  i <- utils::tail(indices, 1L)
  x <- records[[i]]
  if (x$state == "INTENT" || !identical(x$signature, node$definition) ||
      !identical(x$dependencies, node$dependencies) ||
      !identical(x$descriptor, node$descriptor)) stop("Completed bounded node differs.")
  rfbg_proof(control, x, expected = x$proof)
  if (x$state != "JOURNALED") {
    x$state <- "JOURNALED"
    records[[i]] <- x
    rf_write(records, file.path(control$directory, "attempts.rds"))
  }
  invisible(TRUE)
}

rfbg_alignment <- function(control, nodes) {
  records <- rfbg_control(control)
  ids <- names(control$definition$resources)
  dispatched <- vapply(records, function(x) match(x$stage, ids), integer(1))
  if (any(dispatched > length(nodes) + 1L)) {
    stop("Bounded attempt exists beyond pending node.")
  }
  invisible(TRUE)
}

# Pure receipt predicate. File identity and hashes are checked by rfbg_proof.
rfbg_proof_fields <- function(control, attempt, checked, request, declaration,
                              result, request_hash) {
  d <- control$definition
  r <- d$resources[[attempt$stage]]
  directory <- file.path(d$graph_directory, attempt$stage)
  expected_request <- list(
    schema = 1L, spec = attempt$signature$spec,
    expected_definition = attempt$signature, directory = directory,
    resume = attempt$resume, reconciled_attempt = attempt$retry$reconciled_attempt,
    reconciliation = attempt$retry$reconciliation, libraries = d$libraries,
    package_path = checked$package_path, package_paths = checked$packages,
    runtime_sha256 = d$runtime_files, runtime_observation = d$runtime_observation,
    channel_script = checked$files[2]
  )
  if (!identical(request, expected_request)) stop("Supervisor request differs.")
  dynamic <- c("token", "client_writer_inode", "control_read_inode", "status_write_inode")
  if (!is.list(declaration) || is.object(declaration) ||
      !is.null(dim(declaration)) || anyDuplicated(names(declaration))) {
    stop("Malformed supervisor declaration.")
  }
  for (key in dynamic) {
    x <- declaration[[key]]
    pattern <- if (key == "token") "^[a-f0-9]{64}$" else "^[0-9]+$"
    if (!is.character(x) || is.object(x) || !is.null(dim(x)) ||
        length(x) != 1L || is.na(x) || !grepl(pattern, x)) {
      stop("Invalid supervisor channel identity.")
    }
  }
  expected_declaration <- list(
    mode = "worker", libraries = d$libraries, package_paths = checked$packages,
    executor_script = checked$files[1], command = r$rscript,
    args = c("--vanilla", checked$files[5],
             file.path(attempt$supervisor, "request.rds"), request_hash),
    wd = dirname(directory), runtime_sha256 = d$runtime_files,
    all_source_sha256 = d$runtime_files, temp_directory = r$temp_directory,
    cache_directory = r$cache_directory, deadline_seconds = r$timeout_seconds,
    address_space_bytes = r$address_space_bytes, nice = r$nice, threads = r$threads,
    environment = c(R_LIBS = paste(d$libraries, collapse = .Platform$path.sep)),
    prlimit = r$prlimit, nice_command = r$nice_command,
    client = attempt$client_identity, client_time = attempt$client_time,
    token = declaration$token, directory = attempt$supervisor,
    client_writer_inode = declaration$client_writer_inode,
    control_read_inode = declaration$control_read_inode,
    status_write_inode = declaration$status_write_inode,
    channel_script = checked$files[2], guardian_script = checked$files[4],
    source_sha256 = d$runtime_files
  )
  if (!identical(declaration, expected_declaration)) {
    stop("Supervisor declaration differs.")
  }
  guardian_keys <- c("schema", "status", "reason", "execution", "artifact_verified",
                     "worker_started", "client_alive", "guardian_identity",
                     "liveness_observations")
  execution_keys <- c("schema", "status", "diagnostic", "cleanup_error",
                      "post_pin_error", "exit_code", "killed_pids", "elapsed_seconds",
                      "artifact_verified", "generation_selector_invoked",
                      "owned_observation", "cleanup_scope", "runtime_pin_scope")
  if (!rfbg_keys(result, guardian_keys) ||
      !rfbg_keys(result$execution, execution_keys) ||
      !identical(result$schema, 1L) ||
      !identical(result$status, "EXECUTION_SUCCEEDED_UNVERIFIED") ||
      !is.null(result$reason) ||
      !identical(result$artifact_verified, FALSE) ||
      !identical(result$client_alive, TRUE) ||
      !identical(result$worker_started, TRUE) ||
      !is.list(result$execution) || is.object(result$execution) ||
      !identical(result$execution$schema, 1L) ||
      !identical(result$execution$status, "EXECUTION_SUCCEEDED_UNVERIFIED") ||
      !identical(result$execution$exit_code, 0L) ||
      !is.null(result$execution$diagnostic) ||
      !identical(result$execution$artifact_verified, FALSE) ||
      !identical(result$execution$generation_selector_invoked, FALSE) ||
      !is.null(result$execution$cleanup_error) ||
      !is.null(result$execution$post_pin_error) ||
      !is.list(result$execution$owned_observation) ||
      !all(c("owned", "uncertain") %in% names(result$execution$owned_observation)) ||
      length(result$execution$owned_observation$owned) ||
      length(result$execution$owned_observation$uncertain)) {
    stop("Successful guardian cleanup required for READY adoption.")
  }
  invisible(TRUE)
}

rfbg_keys <- function(x, keys) {
  is.list(x) && !is.object(x) && is.null(dim(x)) &&
    identical(sort(names(x)), sort(keys))
}

# Cross-file attempt linkage, using only receipts emitted by the fixed backend.
rfbg_causal_fields <- function(control, attempt, checked, request, declaration,
                               result, request_hash, guardian_ready,
                               worker_declaration, worker_result, child) {
  rfbg_proof_fields(control, attempt, checked, request, declaration, result, request_hash)
  if (!rfbg_keys(guardian_ready, c("pid", "identity", "fd", "client_writer_absent",
                                 "worker_token_absent")) ||
      !rfbg_identity(guardian_ready$identity) ||
      !identical(guardian_ready$identity, result$guardian_identity) ||
      !identical(guardian_ready$pid, guardian_ready$identity$pid) ||
      !identical(guardian_ready$client_writer_absent, TRUE) ||
      !identical(guardian_ready$worker_token_absent, TRUE) ||
      !rfbg_keys(guardian_ready$fd, c("control", "status"))) {
    stop("Guardian readiness identity differs from result.")
  }
  for (role in c("control", "status")) {
    fd <- guardian_ready$fd[[role]]
    inode <- if (role == "control") declaration$control_read_inode else
      declaration$status_write_inode
    number <- if (role == "control") 3L else 4L
    if (!rfbg_keys(fd, c("fd", "flags", "inode")) ||
        !rfbg_number(fd$fd, 0) || fd$fd != number ||
        !identical(fd$inode, inode) || !is.character(fd$flags) ||
        length(fd$flags) != 1L || is.na(fd$flags) ||
        !grepl("^[0-7]+$", fd$flags)) stop("Guardian channel receipt differs.")
    bits <- strtoi(fd$flags, base = 8L)
    if (is.na(bits) || bitwAnd(bits, 2048L) == 0L ||
        bitwAnd(bits, 524288L) == 0L) stop("Guardian channel flags differ.")
  }
  observations <- result$liveness_observations
  if (!is.list(observations) || is.object(observations) || !length(observations) ||
      length(observations) > 64L) stop("Client liveness evidence missing.")
  for (observation in observations) {
    if (!rfbg_keys(observation, c("alive", "first", "last", "handle_running",
                                 "error", "elapsed")) ||
        !identical(observation$alive, TRUE) ||
        !identical(observation$handle_running, TRUE) || !is.null(observation$error) ||
        !rfbg_number(observation$elapsed, 0)) stop("Client liveness evidence differs.")
    for (sample in observation[c("first", "last")]) {
      if (!rfbg_keys(sample, c("identity", "state")) ||
          !identical(sample$identity, attempt$client_identity) ||
          !is.character(sample$state) || length(sample$state) != 1L ||
          is.na(sample$state) || !sample$state %in% c("R", "S", "D", "T", "t", "I", "W", "P")) {
        stop("Client liveness identity differs.")
      }
    }
  }
  if (!identical(worker_result, result$execution)) stop("Worker result differs from guardian.")
  if (!rfbg_keys(child, c("pid", "supervisor_pid", "start_time")) ||
      !identical(child$supervisor_pid, guardian_ready$pid) ||
      !rfbg_number(child$pid, 1) || child$pid != floor(child$pid) ||
      child$pid == child$supervisor_pid || length(child$start_time) != 1L ||
      !typeof(child$start_time) %in% c("integer", "double") ||
      (is.object(child$start_time) &&
       !identical(class(child$start_time), c("POSIXct", "POSIXt"))) ||
      !is.finite(unclass(child$start_time)) || unclass(child$start_time) <= 0) {
    stop("Worker child supervisor identity differs.")
  }
  d <- worker_declaration
  if (!rfbg_keys(d, c("schema", "command", "args", "wd", "environment",
                     "runtime_sha256", "process_baseline", "limits",
                     "capabilities", "artifact_verified")) ||
      !identical(d$schema, 1L) || !identical(d$artifact_verified, FALSE) ||
      !identical(d$capabilities, list(linux = TRUE, processx = TRUE, ps = TRUE,
                                     digest = TRUE)) ||
      !rfbg_keys(d$process_baseline, c("identities", "unknown", "boot_id", "observed_at"))) {
    stop("Worker declaration schema differs.")
  }
  limits <- d$limits
  r <- control$definition$resources[[attempt$stage]]
  if (!rfbg_keys(limits, c("timeout_seconds", "poll_seconds", "wait_seconds",
                          "address_space_bytes", "nice", "nice_plan", "threads",
                          "core_bytes")) ||
      !rfbg_number(limits$timeout_seconds, 0.01) ||
      limits$timeout_seconds > r$timeout_seconds ||
      !identical(limits$poll_seconds, 0.1) || !identical(limits$wait_seconds, 5) ||
      !identical(limits$address_space_bytes, r$address_space_bytes) ||
      !identical(limits$nice, r$nice) || !identical(limits$threads, r$threads) ||
      !identical(limits$core_bytes, 0) ||
      !rfbg_keys(limits$nice_plan, c("inherited", "requested", "increment")) ||
      !rfbg_number(limits$nice_plan$inherited, 0) ||
      limits$nice_plan$inherited != r$nice ||
      !identical(limits$nice_plan$requested, r$nice) ||
      !rfbg_number(limits$nice_plan$increment, 0) || limits$nice_plan$increment != 0) {
    stop("Worker resource limits differ.")
  }
  if (!is.character(d$environment) || is.object(d$environment) ||
      !is.null(dim(d$environment)) || anyNA(d$environment) ||
      is.null(names(d$environment)) || anyNA(names(d$environment)) ||
      anyDuplicated(names(d$environment)) ||
      !"RFX_EXECUTOR_OWNER" %in% names(d$environment)) {
    stop("Worker environment schema differs.")
  }
  owner <- d$environment[["RFX_EXECUTOR_OWNER"]]
  if (!is.character(owner) || is.object(owner) || length(owner) != 1L ||
      is.na(owner) || !grepl("^[a-f0-9]{64}$", owner)) stop("Worker ownership token invalid.")
  expected_environment <- c("current", declaration$environment,
    RFX_FORBIDDEN_FD_INODES = paste(c(declaration$client_writer_inode,
      declaration$control_read_inode, declaration$status_write_inode), collapse = ","),
    RFX_EXECUTOR_OWNER = owner,
    stats::setNames(rep(r$temp_directory, 3L), c("TMPDIR", "TMP", "TEMP")),
    XDG_CACHE_HOME = r$cache_directory,
    stats::setNames(rep(as.character(r$threads), 3L),
                   c("OPENBLAS_NUM_THREADS", "OMP_NUM_THREADS", "MKL_NUM_THREADS")))
  argv <- c(paste0("--as=", format(r$address_space_bytes, scientific = FALSE, trim = TRUE)),
            "--core=0", "--", r$nice_command, "-n", "0", r$rscript, declaration$args)
  if (!identical(d$command, r$prlimit) || !identical(d$args, argv) ||
      !identical(d$wd, declaration$wd) ||
      !identical(d$runtime_sha256, declaration$runtime_sha256) ||
      !identical(d$environment, expected_environment)) {
    stop("Worker dispatch differs from intended guardian request.")
  }
  invisible(TRUE)
}

rfbg_number <- function(x, minimum) {
  typeof(x) %in% c("integer", "double") && !is.object(x) && is.null(dim(x)) &&
    length(x) == 1L && is.finite(x) && x >= minimum
}

rfbg_identity <- function(x) {
  rfbg_keys(x, c("pid", "uid", "start_ticks")) &&
    rfbg_number(x$pid, 1) && x$pid == floor(x$pid) &&
    rfbg_number(x$uid, 0) && x$uid == floor(x$uid) &&
    is.character(x$start_ticks) && !is.object(x$start_ticks) &&
    length(x$start_ticks) == 1L && !is.na(x$start_ticks) &&
    grepl("^[0-9]+$", x$start_ticks)
}
