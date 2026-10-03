# Installed guardian entry point; declarations contain data only.
a <- commandArgs(TRUE)
stopifnot(length(a) == 4L, grepl("^[0-9a-f]{64}$", a[[2]]))
stopifnot(requireNamespace("digest", quietly = TRUE), requireNamespace("processx",
  quietly = TRUE), requireNamespace("ps", quietly = TRUE))
stopifnot(identical(digest::digest(file = a[[1]], algo = "sha256", serialize = FALSE), a[[2]]))
d <- readRDS(a[[1]])
stopifnot(is.list(d), !is.object(d), is.character(d$source_sha256),
  !anyDuplicated(names(d$source_sha256)))
for (f in names(d$source_sha256)) {
  stopifnot(utils::file_test("-f", f), identical(normalizePath(f, mustWork = TRUE), f))
  walk <- f
  repeat {
    link <- Sys.readlink(walk)
    stopifnot(is.na(link) || !nzchar(link))
    if (walk == "/") break
    walk <- dirname(walk)
  }
  stopifnot(identical(digest::digest(file = f, algo = "sha256", serialize = FALSE),
    d$source_sha256[[f]]))
}
entry <- sub("^--file=", "", commandArgs(FALSE)[grep("^--file=", commandArgs(FALSE))][1])
base <- dirname(entry)
stopifnot(
  identical(d$guardian_script, entry),
  identical(d$executor_script, file.path(base, "executor.R")),
  identical(d$channel_script, file.path(base, "channel.R"))
)
keys <- c(
  "mode", "libraries", "package_paths", "executor_script", "command", "args", "wd",
    "runtime_sha256",
  "all_source_sha256", "temp_directory", "cache_directory", "deadline_seconds",
  "address_space_bytes", "nice", "threads", "environment", "prlimit", "nice_command",
  "client", "client_time", "token", "directory", "client_writer_inode",
  "control_read_inode", "status_write_inode", "channel_script", "guardian_script", "source_sha256"
)
stopifnot(
  identical(sort(names(d)), sort(keys)),
  all(c(d$executor_script, d$channel_script, d$guardian_script, file.path(base,
    "schema.R")) %in% names(d$source_sha256))
)
validation <- new.env(parent = baseenv())
sys.source(file.path(base, "schema.R"), envir = validation)
stopifnot(
  validation$rb_keys(d, keys), identical(d$mode, "worker"),
  identical(d$source_sha256, d$all_source_sha256),
  identical(d$source_sha256, d$runtime_sha256),
  identical(names(d$client), c("pid", "uid", "start_ticks"))
)
stopifnot(identical(.libPaths(), d$libraries))
for (name in names(d$package_paths)) {
  stopifnot(identical(find.package(name), d$package_paths[[name]]))
  if (isNamespaceLoaded(name)) stopifnot(identical(if (identical(name,
    "base")) file.path(R.home("library"), "base") else getNamespaceInfo(asNamespace(name),
    "path"), d$package_paths[[name]]))
}
source(d$executor_script, local = .GlobalEnv)
source(d$channel_script, local = .GlobalEnv)
rfx_path(d$directory, directory = TRUE)
rfx_path(d$temp_directory, directory = TRUE)
rfx_path(d$cache_directory, directory = TRUE)
stopifnot(rfx_number(d$deadline_seconds, 1, 60), identical(d$mode, "worker"))
fds <- as.integer(a[3:4])
stopifnot(identical(fds, c(3L, 4L)))
input <- processx::conn_create_fd(fds[[1]])
output <- processx::conn_create_fd(fds[[2]])
channel <- rg_state(input, output, d$token, a[[2]])
processx::conn_disable_inheritance()
fd_record <- list(control = rg_fd_flags(input), status = rg_fd_flags(output))
stopifnot(identical(fd_record$control$inode, d$control_read_inode),
  identical(fd_record$status$inode, d$status_write_inode))
rg_assert_no_inodes(d$client_writer_inode)
stopifnot(bitwAnd(strtoi(fd_record$control$flags, 8L), 524288L) != 0L,
  bitwAnd(strtoi(fd_record$status$flags, 8L), 524288L) != 0L)
lock <- file.path(d$directory, "guardian.lock")
stopifnot(dir.create(lock, showWarnings = FALSE))
handle <- ps::ps_handle(d$client$pid, d$client_time)
liveness_observations <- list()
identity_alive <- function() {
  observed <- rg_observe_client(d$client, handle)
  # Accumulate liveness observations in the enclosing guardian scope.
  liveness_observations[[length(liveness_observations) + 1L]] <<- # nolint: assignment_linter.
    observed
  if (length(liveness_observations) > 64L) liveness_observations <<- liveness_observations[c(1:4,
    tail(seq_along(liveness_observations), 60L))]
  isTRUE(observed$alive)
}
deadline <- proc.time()[["elapsed"]] + d$deadline_seconds
rfx_new_rds(list(
  pid = Sys.getpid(), identity = rg_stable_identity(Sys.getpid()), fd = fd_record,
  client_writer_absent = TRUE, worker_token_absent = !nzchar(Sys.getenv("RFX_EXECUTOR_OWNER"))
), file.path(d$directory, "guardian-ready.rds"))
status <- "GUARDIAN_FAILED"
reason <- NULL
execution <- NULL
started <- FALSE
running_sent <- FALSE
check <- function() {
  if (!identity_alive()) {
    reason <<- "CLIENT_LOST"
    return(TRUE)
  }
  if (proc.time()[["elapsed"]] >= deadline) {
    reason <<- "DEADLINE"
    return(TRUE)
  }
  messages <- rg_receive(channel, 0L)
  if (channel$eof) {
    reason <<- "CONTROL_EOF"
    return(TRUE)
  }
  if (length(messages)) {
    if (identical(messages, "CANCEL")) {
      reason <<- "CANCELLED"
      return(TRUE)
    }
    stop("Unexpected control frame after START")
  }
  worker_identity <- file.path(d$directory, "worker/child.rds")
  if (started && !running_sent && file.exists(worker_identity)) {
    rg_send(channel, "RUNNING", deadline, identity_alive)
    running_sent <<- TRUE
  }
  FALSE
}
tryCatch(
  {
    if (!identity_alive()) stop("Client lost before READY")
    rg_send(channel, "READY", deadline, identity_alive)
    repeat {
      if (!identity_alive()) {
        reason <- "CLIENT_LOST"
        stop(reason)
      }
      if (proc.time()[["elapsed"]] >= deadline) {
        reason <- "DEADLINE"
        stop(reason)
      }
      got <- rg_receive(channel, 50L)
      if (channel$eof) {
        reason <- "CONTROL_EOF"
        stop(reason)
      }
      if (length(got)) {
        if (!identical(got, "START")) stop("Expected one START")
        break
      }
    }
    if (d$mode == "probe") {
      if (check()) stop(reason)
      status <- "CHANNEL_PROBE_SUCCESS"
    } else {
      started <- TRUE
      env <- c(d$environment, RFX_FORBIDDEN_FD_INODES = paste(c(d$client_writer_inode,
        d$control_read_inode, d$status_write_inode), collapse = ","))
      execution <- rfx_execute(d$command, d$args, file.path(d$directory, "worker"), d$wd,
        d$runtime_sha256,
        d$temp_directory, d$cache_directory, max(.01, deadline - proc.time()[["elapsed"]]),
        cancel = check,
        address_space_bytes = d$address_space_bytes, nice = d$nice, threads = d$threads,
          environment = env,
        prlimit = d$prlimit, nice_command = d$nice_command
      )
      if (!identity_alive()) reason <- "CLIENT_LOST"
      status <- if (!is.null(reason)) paste0(reason, "_",
        if (is.null(execution$cleanup_error)) "CLEANUP_COMPLETE" else "CLEANUP_UNRESOLVED"
      ) else execution$status
    }
  },
  error = function(e) {
    if (is.null(reason)) reason <<- conditionMessage(e)
    status <<- "GUARDIAN_FAILED"
  }
)
# No live original client => no success, even if worker completed concurrently.
final_alive <- identity_alive()
if (!final_alive) {
  reason <- "CLIENT_LOST"
  if (!grepl("CLIENT_LOST", status)) status <- "CLIENT_LOST"
}
result <- list(
  schema = 1L, status = status, reason = reason, execution = execution, artifact_verified = FALSE,
  worker_started = started, client_alive = final_alive,
    guardian_identity = rg_stable_identity(Sys.getpid()),
  liveness_observations = liveness_observations
)
rfx_new_rds(result, file.path(d$directory, "guardian-result.rds"))
success <- status %in% c("CHANNEL_PROBE_SUCCESS", "EXECUTION_SUCCEEDED_UNVERIFIED") && final_alive
if (success) {
  rg_send(channel, "FINISHED", deadline, identity_alive)
  unlink(lock, recursive = TRUE)
}
processx::processx_conn_close(input)
processx::processx_conn_close(output)
quit(status = if (success) 0L else 2L)
