# Execution receipts do not establish artifact verification.
rfx_text <- function(x) {
  is.character(x) && !is.object(x) && is.null(dim(x)) &&
    length(x) == 1L && !is.na(x) && nzchar(x)
}
rfx_number <- function(x, lower, upper = Inf, integral = FALSE) {
  typeof(x) %in% c("double", "integer") && !is.object(x) &&
    is.null(dim(x)) && length(x) == 1L && is.finite(x) &&
    x >= lower && x <= upper && (!integral || x == floor(x))
}
rfx_path <- function(x, exists = TRUE, directory = FALSE) {
  if (!rfx_text(x) || !startsWith(x, "/") || grepl("[\r\n]", x)) {
    stop("Absolute plain path required.")
  }
  walk <- x
  repeat {
    link <- Sys.readlink(walk)
    if (!is.na(link) && nzchar(link)) stop("Symlink path refused.")
    if (identical(walk, "/")) break
    next_walk <- dirname(walk)
    if (identical(next_walk, walk)) stop("Invalid ancestor.")
    walk <- next_walk
  }
  parent <- normalizePath(dirname(x), mustWork = TRUE)
  canonical <- file.path(parent, basename(x))
  if (identical(x, "/")) canonical <- "/"
  if (!identical(x, canonical)) stop("Canonical path required.")
  if (exists) {
    if (directory) {
      if (!dir.exists(x)) stop("Directory required.")
    } else if (!utils::file_test("-f", x)) {
      stop("Regular file required.")
    }
  }
  x
}
rfx_hashes <- function(files) {
  if (!is.character(files) || is.object(files) || !is.null(dim(files)) ||
    anyNA(files) || anyDuplicated(files)) {
    stop("Unique runtime files required.")
  }
  stats::setNames(vapply(files, function(x) {
    rfx_path(x)
    digest::digest(file = x, algo = "sha256", serialize = FALSE)
  }, ""), files)
}
rfx_new_rds <- function(value, path) {
  link <- Sys.readlink(path)
  if (file.exists(path) || (!is.na(link) && nzchar(link))) {
    stop("Receipt already exists.")
  }
  temporary <- paste0(path, ".writing")
  tmp_link <- Sys.readlink(temporary)
  if (file.exists(temporary) || (!is.na(tmp_link) &&
    nzchar(tmp_link))) stop("Prior receipt partial refused.")
  saveRDS(value, temporary, version = 3)
  if (!file.link(temporary, path)) stop("Exclusive receipt publication failed.")
  unlink(temporary)
}
rfx_capabilities <- function() {
  list(
    linux = identical(Sys.info()[["sysname"]], "Linux"),
    processx = requireNamespace("processx", quietly = TRUE),
    ps = requireNamespace("ps", quietly = TRUE),
    digest = requireNamespace("digest", quietly = TRUE)
  )
}
# Invocation-local immutable process identities; no historical lock adoption.
rfx_identity <- function(pid) {
  base <- file.path("/proc", as.character(pid))
  before <- file.info(base)$uid
  fields <- strsplit(sub("^.*\\) ", "", readLines(file.path(base, "stat"), warn = FALSE)),
    " +")[[1]]
  after <- file.info(base)$uid
  if (length(fields) < 20L || is.na(before) || is.na(after) || before != after ||
    !grepl("^[0-9]+$", fields[[20]])) {
    stop("Unknown process identity.")
  }
  list(
    identity = list(pid = as.character(pid), uid = before, start_ticks = fields[[20]]),
    state = fields[[1]]
  )
}
rfx_baseline <- function() {
  uid <- file.info("/proc/self")$uid
  identities <- list()
  unknown <- character()
  for (pid in list.files("/proc", pattern = "^[0-9]+$")) {
    item <- tryCatch(
      {
        first <- rfx_identity(pid)
        second <- rfx_identity(pid)
        if (!identical(first$identity, second$identity)) stop("Identity changed.")
        if (first$identity$uid == uid) first$identity else NULL
      },
      error = function(e) {
        if (dir.exists(file.path("/proc", pid))) unknown <<- c(unknown, pid)
        NULL
      }
    )
    if (!is.null(item)) identities[[pid]] <- item
  }
  list(
    identities = identities, unknown = unknown,
    boot_id = readLines("/proc/sys/kernel/random/boot_id", warn = FALSE),
    observed_at = as.character(Sys.time())
  )
}
rfx_classify_observation <- function(before, after, kind, fields, token, baseline) {
  if (is.null(before) || is.null(after) || !identical(before, after)) {
    return("uncertain")
  }
  if (identical(kind, "readable")) {
    if (paste0("RFX_EXECUTOR_OWNER=", token) %in% fields) {
      return("owned")
    }
    return("other")
  }
  if (identical(kind, "unreadable") &&
    identical(baseline$identities[[before$pid]], before)) {
    return("preexisting")
  }
  "uncertain"
}
rfx_owned_observation <- function(token, baseline) {
  uid <- file.info("/proc/self")$uid
  owned <- list()
  uncertain <- character()
  excluded <- list()
  for (pid in list.files("/proc", pattern = "^[0-9]+$")) {
    base <- file.path("/proc", pid)
    info <- file.info(base)
    if (!is.na(info$uid) && info$uid != uid) next
    first <- tryCatch(rfx_identity(pid), error = function(e) NULL)
    if (is.null(first)) {
      if (dir.exists(base)) uncertain <- c(uncertain, pid)
      next
    }
    if (identical(first$state, "Z")) next
    environment <- tryCatch(
      {
        con <- suppressWarnings(file(file.path(base, "environ"), "rb"))
        raw <- tryCatch(readBin(con, "raw", n = 1048577L), finally = close(con))
        if (length(raw) > 1048576L) {
          list(kind = "invalid", fields = character())
        } else {
          ends <- which(raw == as.raw(0))
          start <- 1L
          fields <- character()
          for (end in ends) {
            if (end > start) fields <- c(fields, rawToChar(raw[start:(end - 1L)]))
            start <- end + 1L
          }
          list(kind = if (start <= length(raw)) "invalid" else "readable", fields = fields)
        }
      },
      error = function(e) list(kind = "unreadable", fields = character())
    )
    last <- tryCatch(rfx_identity(pid), error = function(e) NULL)
    if (is.null(last) && !dir.exists(base)) next
    verdict <- rfx_classify_observation(
      first$identity, last$identity,
      environment$kind, environment$fields, token, baseline
    )
    if (verdict == "owned") owned[[pid]] <- first$identity
    if (verdict == "preexisting") excluded[[pid]] <- first$identity
    if (verdict == "uncertain") uncertain <- c(uncertain, pid)
  }
  list(owned = owned, uncertain = uncertain, excluded_preexisting = excluded)
}
rfx_nice_plan <- function(inherited, requested) {
  if (!rfx_number(inherited, -20, 19, TRUE) || !rfx_number(requested, 0, 19, TRUE)) {
    stop("Invalid absolute priority request.")
  }
  if (requested < inherited) stop("Requested priority elevation is unsupported.")
  list(inherited = inherited, requested = requested, increment = requested - inherited)
}
rfx_execute <- function(command, args, directory, wd, runtime_sha256,
                        temp_directory, cache_directory, timeout_seconds,
                        cancel = function() FALSE, poll_seconds = 0.1,
                        wait_seconds = 5, address_space_bytes = 8 * 1024^3,
                        nice = 10L, threads = 2L,
                        prlimit = "/usr/bin/prlimit", nice_command = "/usr/bin/nice",
                        environment = character()) {
  capabilities <- rfx_capabilities()
  if (!all(unlist(capabilities)) || !ps::ps_is_supported()) {
    stop("Required Linux/processx/ps/digest capability unavailable.")
  }
  if (!rfx_number(timeout_seconds, 0.01) ||
    !rfx_number(poll_seconds, 0.01, 1) ||
    !rfx_number(wait_seconds, 0.1, 30) ||
    !rfx_number(address_space_bytes, 1, 2^53, TRUE) ||
    !rfx_number(nice, 0, 19, TRUE) || !rfx_number(threads, 1, 2, TRUE) ||
    !is.function(cancel)) {
    stop("Invalid execution limits/callback.")
  }
  if (!is.character(args) || is.object(args) || !is.null(dim(args)) || anyNA(args)) {
    stop("Plain argument vector required.")
  }
  if (!is.character(environment) || is.object(environment) ||
    !is.null(dim(environment)) || anyNA(environment) ||
    (length(environment) && (is.null(names(environment)) ||
      anyNA(names(environment)) || any(!nzchar(names(environment))) ||
      anyDuplicated(names(environment))))) {
    stop("Invalid environment.")
  }
  reserved <- c(
    "TMPDIR", "TMP", "TEMP", "XDG_CACHE_HOME", "OPENBLAS_NUM_THREADS",
    "OMP_NUM_THREADS", "MKL_NUM_THREADS"
  )
  if (any(names(environment) %in% c(reserved,
    "RFX_EXECUTOR_OWNER"))) stop("Reserved environment override.")
  nice_plan <- rfx_nice_plan(ps::ps_get_nice(ps::ps_handle()), nice)
  command <- rfx_path(command)
  prlimit <- rfx_path(prlimit)
  nice_command <- rfx_path(nice_command)
  if (any(file.access(c(command, prlimit, nice_command), 1) != 0)) {
    stop("Executable capability unavailable.")
  }
  wd <- rfx_path(wd, directory = TRUE)
  temp_directory <- rfx_path(temp_directory, directory = TRUE)
  cache_directory <- rfx_path(cache_directory, directory = TRUE)
  directory <- rfx_path(directory, exists = FALSE)
  if (!is.character(runtime_sha256) || is.object(runtime_sha256) ||
    !is.null(dim(runtime_sha256)) || is.null(names(runtime_sha256)) ||
    anyDuplicated(names(runtime_sha256)) || anyNA(runtime_sha256) ||
    !all(grepl("^[0-9a-f]{64}$", runtime_sha256)) ||
    !all(c(command, prlimit, nice_command) %in% names(runtime_sha256))) {
    stop("Declared executable/runtime pins required.")
  }
  if (!identical(rfx_hashes(names(runtime_sha256)), runtime_sha256)) {
    stop("Prelaunch runtime pins differ.")
  }
  cancelled <- function() {
    value <- cancel()
    if (!is.logical(value) || length(value) != 1L || is.na(value) ||
      !is.null(dim(value)) || is.object(value)) {
      stop("Invalid cancellation result.")
    }
    isTRUE(value)
  }
  if (cancelled()) stop("Cancelled before launch.")
  if (file.exists(directory) || !dir.create(directory, showWarnings = FALSE)) {
    stop("Fresh supervisor directory required; explicit reconciliation only.")
  }
  lock <- file.path(directory, "supervisor.lock")
  if (!dir.create(lock)) stop("Supervisor lock unavailable.")
  child <- NULL
  settled <- FALSE
  baseline <- rfx_baseline()
  token <- digest::digest(list(Sys.getpid(), Sys.time(), tempfile()), algo = "sha256")
  observation <- NULL
  cleanup <- function() {
    if (is.null(child)) {
      return(integer())
    }
    killed <- child$kill_tree()
    child$wait(timeout = wait_seconds * 1000)
    if (child$is_alive()) stop("Owned child did not terminate; reconciliation required.")
    deadline <- proc.time()[["elapsed"]] + wait_seconds
    repeat {
      # Accumulate cleanup observations in the enclosing invocation.
      observation <<- rfx_owned_observation(token, baseline) # nolint: assignment_linter.
      if (!length(observation$owned) && !length(observation$uncertain)) break
      if (proc.time()[["elapsed"]] >= deadline) {
        stop("Descendant cleanup observation unresolved; lock retained.")
      }
      Sys.sleep(min(poll_seconds, 0.1))
    }
    killed
  }
  on.exit(
    {
      if (!settled && !is.null(child)) try(cleanup(), silent = TRUE)
      # Retain lock on unexpected exit; no automatic stale-lock recovery.
    },
    add = TRUE
  )
  env <- c("current", environment,
    RFX_EXECUTOR_OWNER = token, stats::setNames(rep(temp_directory, 3), c("TMPDIR", "TMP", "TEMP")),
    XDG_CACHE_HOME = cache_directory,
    stats::setNames(rep(as.character(threads), 3), reserved[5:7])
  )
  argv <- c(
    paste0("--as=", format(address_space_bytes, scientific = FALSE, trim = TRUE)),
    "--core=0", "--", nice_command, "-n", as.character(nice_plan$increment), command, args
  )
  declaration <- list(
    schema = 1L, command = prlimit, args = argv,
    wd = wd, environment = env, runtime_sha256 = runtime_sha256,
    process_baseline = baseline,
    limits = list(
      timeout_seconds = timeout_seconds, poll_seconds = poll_seconds,
      wait_seconds = wait_seconds, address_space_bytes = address_space_bytes,
      nice = nice, nice_plan = nice_plan, threads = threads, core_bytes = 0
    ), capabilities = capabilities,
    artifact_verified = FALSE
  )
  rfx_new_rds(declaration, file.path(directory, "declaration.rds"))
  status <- "LAUNCH_FAILED"
  diagnostic <- NULL
  killed <- integer()
  code <- NULL
  started <- proc.time()[["elapsed"]]
  tryCatch(
    {
      child <- processx::process$new(prlimit, argv,
        wd = wd, env = env,
        stdout = file.path(directory, "stdout.log"), stderr = file.path(directory, "stderr.log"),
        cleanup = TRUE, cleanup_tree = TRUE, supervise = TRUE, linux_pdeathsig = TRUE
      )
      rfx_new_rds(
        list(
          pid = child$get_pid(), supervisor_pid = Sys.getpid(),
          start_time = child$get_start_time()
        ),
        file.path(directory, "child.rds")
      )
      repeat {
        if (cancelled()) {
          status <- "CANCELLED"
          break
        }
        if (proc.time()[["elapsed"]] - started >= timeout_seconds) {
          status <- "TIMED_OUT"
          break
        }
        if (!child$is_alive()) {
          code <- child$get_exit_status()
          status <- if (identical(code, 0L)) "EXECUTION_SUCCEEDED_UNVERIFIED" else "CHILD_FAILED"
          break
        }
        Sys.sleep(poll_seconds)
      }
    },
    interrupt = function(e) {
      status <<- "INTERRUPTED"
      diagnostic <<- conditionMessage(e)
    },
    error = function(e) {
      status <<- "EXECUTION_ERROR"
      diagnostic <<- conditionMessage(e)
    }
  )
  cleanup_error <- tryCatch(
    {
      killed <- cleanup()
      NULL
    },
    error = conditionMessage
  )
  if (!is.null(cleanup_error)) {
    status <- "CLEANUP_UNRESOLVED"
    diagnostic <- cleanup_error
  }
  post_error <- tryCatch(
    {
      if (!identical(rfx_hashes(names(runtime_sha256)), runtime_sha256)) stop("Runtime changed.")
      NULL
    },
    error = conditionMessage
  )
  if (!is.null(post_error)) {
    status <- "FINAL_PIN_FAILURE"
    diagnostic <- post_error
  }
  result <- list(
    schema = 1L, status = status, diagnostic = diagnostic,
    cleanup_error = cleanup_error, post_pin_error = post_error,
    exit_code = code, killed_pids = killed, elapsed_seconds = proc.time()[["elapsed"]] - started,
    artifact_verified = FALSE, generation_selector_invoked = FALSE,
    owned_observation = observation,
    cleanup_scope = "processx environment-tagged tree; not a sandbox or marker-erasure containment",
    runtime_pin_scope = "Only explicitly declared files, not whole process/ELF closure"
  )
  rfx_new_rds(result, file.path(directory, "result.rds"))
  if (is.null(cleanup_error)) {
    unlink(lock, recursive = TRUE)
    settled <- TRUE
  }
  result
}
