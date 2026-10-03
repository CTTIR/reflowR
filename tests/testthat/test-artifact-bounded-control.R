# These tests exercise parent control flow without launching a process.
# Installed lifecycle tests separately establish real channel/tree behavior.
bounded_control_fixture <- function(frame = parent.frame()) {
  skip_if_not(identical(Sys.info()[["sysname"]], "Linux"))
  skip_if_not_installed("processx")
  skip_if_not_installed("ps")
  root <- tempfile(tmpdir = normalizePath(tempdir(), mustWork = TRUE))
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE), envir = frame)
  withr::local_envvar(c(OPENBLAS_NUM_THREADS = "2", OMP_NUM_THREADS = "2",
    MKL_NUM_THREADS = "2"), .local_envir = frame)
  scripts <- system.file("bounded", package = "reflowR", mustWork = TRUE)
  files <- file.path(scripts, c("executor.R", "channel.R", "client.R",
    "guardian.R", "worker.R", "schema.R"))
  executable <- normalizePath(file.path(R.home("bin"), "Rscript"), mustWork = TRUE)
  pins <- vapply(unique(c(files, executable)), function(f) {
    digest::digest(file = f, algo = "sha256", serialize = FALSE)
  }, character(1))
  resources <- list(timeout_seconds = 1, address_space_bytes = 8 * 1024^3,
    nice = 19, threads = 2L, temp_directory = root, cache_directory = root,
    rscript = executable, prlimit = executable, nice_command = executable)
  spec <- reflow_artifact_spec("base", "writeLines", list(text = "literal"),
    outputs = list(con = list(path = "x.txt", type = "file")),
    inventory = c(x = "x.txt"))
  state <- new.env(parent = emptyenv())
  state$started <- 0L
  state$messages <- character()
  state$alive_calls <- 0L
  state$alive_until <- 1L
  state$wait_error <- NULL
  state$receive_error <- NULL
  state$result <- list(status = "EXECUTION_SUCCEEDED_UNVERIFIED",
    client_alive = TRUE, worker_started = TRUE,
    execution = list(cleanup_error = NULL, post_pin_error = NULL,
      owned_observation = list(owned = list(), uncertain = list())))
  state$answer <- list(independently_expected_descriptor = TRUE)
  state$verify_error <- FALSE
  state$plain <- TRUE
  original_source <- base::sys.source
  control <- new.env(parent = asNamespace("reflowR"))
  for (name in c("rfb_run", "reflow_artifact_run_bounded",
    "reflow_artifact_resume_bounded")) {
    original <- get(name, envir = asNamespace("reflowR"))
    copied <- original
    environment(copied) <- control
    stopifnot(identical(body(copied), body(original)),
      identical(formals(copied), formals(original)))
    assign(name, copied, envir = control)
  }
  bindings <- list(
    rfb_packages = function(roots) character(),
    rfb_ready_only = function(spec, directory, expected_definition) {
      state$verified_definition <- expected_definition
      state$answer
    },
    sys.source = function(file, envir, ...) {
      original_source(file, envir, ...)
      if (basename(file) != "schema.R") return(invisible(NULL))
      envir$rb_plain <- function(x) state$plain
      envir$rb_request <- function(x) {
        state$request <- x
        TRUE
      }
      envir$rg_client_start <- function(declaration, directory, ...) {
        state$started <- state$started + 1L
        state$declaration <- declaration
        dir.create(directory)
        saveRDS(state$result, file.path(directory, "guardian-result.rds"))
        list(channel = list(write = 1L, read = 2L), process = list(is_alive = function() {
          state$alive_calls <- state$alive_calls + 1L
          state$alive_calls <= state$alive_until
        }))
      }
      envir$rg_client_wait <- function(...) {
        if (!is.null(state$wait_error)) stop(state$wait_error)
        invisible(NULL)
      }
      envir$rg_send <- function(channel, message, ...) {
        state$messages <- c(state$messages, message)
      }
      envir$rg_receive <- function(...) {
        if (!is.null(state$receive_error)) stop(state$receive_error)
        invisible(NULL)
      }
      original_hashes <- envir$rfx_hashes
      envir$rfx_hashes <- function(files) {
        if (state$verify_error) stop("controlled runtime drift")
        original_hashes(files)
      }
    })
  list2env(bindings, envir = control)
  testthat::local_mocked_bindings(processx_conn_close = function(...) NULL,
    .package = "processx", .env = frame)
  list(root = root, resources = resources, spec = spec, pins = pins, state = state,
    control = control,
    directory = file.path(root, "artifact"), supervisor = file.path(root, "supervisor"))
}

bounded_control_run <- function(f, cancel = function() FALSE, resume = FALSE) {
  if (resume) {
    f$control$reflow_artifact_resume_bounded(f$spec, f$directory, f$supervisor,
      f$resources, f$pins, cancel, "prior", list(reason = "explicit"))
  } else {
    f$control$reflow_artifact_run_bounded(f$spec, f$directory, f$supervisor,
      f$resources, f$pins, cancel)
  }
}

test_that("parent sends START and verifies the same captured definition", {
  f <- bounded_control_fixture()
  expected <- rfa_signature(f$spec)
  expect_identical(bounded_control_run(f), f$state$answer)
  expect_identical(f$state$messages, "START")
  expect_identical(f$state$started, 1L)
  expect_identical(f$state$request$expected_definition, expected)
  expect_identical(f$state$verified_definition, expected)
  expect_identical(readRDS(file.path(f$supervisor, "artifact-verified.rds"))$status,
    "ARTIFACT_VERIFIED")
})

test_that("resume forwards explicit reconciliation without another baseline", {
  f <- bounded_control_fixture()
  dir.create(f$directory)
  expect_identical(bounded_control_run(f, resume = TRUE), f$state$answer)
  expect_true(f$state$request$resume)
  expect_identical(f$state$request$reconciled_attempt, "prior")
  expect_identical(f$state$request$reconciliation, list(reason = "explicit"))
})

test_that("invalid or true prelaunch cancellation never starts the backend", {
  f <- bounded_control_fixture()
  for (value in list(NA, 1, logical(), c(TRUE, FALSE), structure(TRUE, class = "custom"))) {
    expect_error(bounded_control_run(f, function() value), "Invalid cancellation")
  }
  expect_error(bounded_control_run(f, function() TRUE), "Cancelled before launch")
  expect_error(bounded_control_run(f, NULL), "Cancellation callback required")
  expect_identical(f$state$started, 0L)
  expect_false(dir.exists(f$supervisor))
})

test_that("runtime pins and data-only declarations refuse before launch", {
  f <- bounded_control_fixture()
  saved <- f$pins
  f$pins <- saved[-1]
  expect_error(bounded_control_run(f), "Complete declared runtime")
  f$pins <- saved
  f$pins[[1]] <- strrep("0", 64)
  expect_error(bounded_control_run(f), "Runtime pin mismatch")
  f$pins <- saved
  f$state$plain <- FALSE
  expect_error(bounded_control_run(f), "Data-only")
  expect_identical(f$state$started, 0L)
})

test_that("overlapping and existing outputs refuse without backend activity", {
  f <- bounded_control_fixture()
  original <- f$supervisor
  f$supervisor <- f$directory
  expect_error(bounded_control_run(f), "overlap")
  f$supervisor <- original
  dir.create(f$supervisor)
  expect_error(bounded_control_run(f), "Fresh destination")
  expect_identical(f$state$started, 0L)
})

test_that("changed preflight signature cannot become a new accepted definition", {
  f <- bounded_control_fixture()
  expected <- rfa_signature(f$spec)
  calls <- 0L
  testthat::local_mocked_bindings(rfa_signature = function(spec) {
    calls <<- calls + 1L
    if (calls == 1L) expected else list(changed = TRUE)
  }, .package = "reflowR")
  expect_error(bounded_control_run(f), "changed during preflight")
  expect_identical(f$state$started, 0L)
})

test_that("callback errors record failed client and cannot produce verified receipt", {
  f <- bounded_control_fixture()
  calls <- 0L
  cancel <- function() {
    calls <<- calls + 1L
    if (calls > 1L) stop("literal callback failure")
    FALSE
  }
  expect_error(bounded_control_run(f, cancel), "literal callback failure")
  diagnostic <- readRDS(file.path(f$supervisor, "client-failure.rds"))
  expect_identical(diagnostic$status, "CLIENT_FAILED")
  expect_identical(diagnostic$error, "literal callback failure")
  expect_false(diagnostic$guardian_alive)
  expect_false(file.exists(file.path(f$supervisor, "artifact-verified.rds")))
})

test_that("postlaunch cancellation sends exactly one CANCEL", {
  f <- bounded_control_fixture()
  f$state$alive_until <- 3L
  count <- 0L
  cancel <- function() {
    count <<- count + 1L
    count > 1L
  }
  expect_identical(bounded_control_run(f, cancel), f$state$answer)
  expect_identical(f$state$messages, c("START", "CANCEL"))
})

test_that("runtime drift after guardian completion blocks READY verification", {
  f <- bounded_control_fixture()
  f$state$verify_error <- TRUE
  expect_error(bounded_control_run(f), "controlled runtime drift")
  expect_null(f$state$verified_definition)
  expect_false(file.exists(file.path(f$supervisor, "artifact-verified.rds")))
})

test_that("guardian failure evidence is retained without verified output", {
  f <- bounded_control_fixture()
  f$state$result$execution$owned_observation$uncertain <- list(unresolved = TRUE)
  expect_error(bounded_control_run(f), "Bounded execution failed")
  expect_true(file.exists(file.path(f$supervisor, "guardian-result.rds")))
  expect_false(file.exists(file.path(f$supervisor, "artifact-verified.rds")))
})

test_that("guardian handshake failure records literal failure after cleanup", {
  f <- bounded_control_fixture()
  f$state$wait_error <- "literal handshake failure"
  f$state$alive_until <- 0L
  expect_error(bounded_control_run(f), "literal handshake failure")
  expect_identical(f$state$messages, character())
  expect_identical(readRDS(file.path(f$supervisor, "client-failure.rds"))$error,
    "literal handshake failure")
})

test_that("expired parent cleanup bound fails rather than accepting later result", {
  f <- bounded_control_fixture()
  clock <- 0L
  f$control$proc.time <- function() {
    clock <<- clock + 100L
    c(user.self = 0, sys.self = 0, elapsed = clock)
  }
  expect_error(bounded_control_run(f), "Guardian cleanup unresolved")
  expect_false(file.exists(file.path(f$supervisor, "artifact-verified.rds")))
})

test_that("tracked input cannot contain the supervisor", {
  f <- bounded_control_fixture()
  input <- file.path(f$root, "tracked")
  dir.create(input)
  writeLines("literal", file.path(input, "content"))
  f$spec$args$text <- reflow_imaging_input(input)
  f$supervisor <- file.path(input, "supervisor")
  expect_error(bounded_control_run(f), "Supervisor overlaps tracked input")
  expect_identical(f$state$started, 0L)
})

test_that("namespace selection refuses a loaded package at a different path", {
  control <- new.env(parent = asNamespace("reflowR"))
  selected <- rfb_packages
  environment(selected) <- control
  expect_identical(body(selected), body(rfb_packages))
  expect_identical(formals(selected), formals(rfb_packages))
  control$find.package <- function(...) "/different"
  expect_error(selected("base"), "Loaded namespace differs")
})

test_that("dependency traversal deduplicates roots and includes declared imports", {
  skip_if_not_installed("digest")
  paths <- rfb_packages(c("digest", "base", "digest"))
  expect_false(anyDuplicated(names(paths)) > 0L)
  expect_true(all(c("digest", "base") %in% names(paths)))
  expect_identical(paths[["digest"]], find.package("digest"))
})

test_that("capability refusal precedes all declaration processing", {
  control <- new.env(parent = asNamespace("reflowR"))
  capability <- rfb_capability
  environment(capability) <- control
  expect_identical(body(capability), body(rfb_capability))
  expect_identical(formals(capability), formals(rfb_capability))
  control$Sys.info <- function() c(sysname = "unsupported")
  expect_error(capability(), "capability unavailable")
})

test_that("READY lock refusal does not clear another verifier's lock", {
  f <- bounded_control_fixture()
  dir.create(f$directory)
  lock <- file.path(f$directory, ".lock")
  dir.create(lock)
  expect_error(rfb_ready_only(f$spec, f$directory, rfa_signature(f$spec)),
    "Artifact verification locked")
  expect_true(dir.exists(lock))
  expect_identical(list.files(f$directory, all.files = TRUE, no.. = TRUE), ".lock")
})

test_that("READY revalidation refuses drift after descriptor verification", {
  f <- bounded_control_fixture()
  expected <- rfa_signature(f$spec)
  reflow_artifact_run(f$spec, f$directory)
  inventory <- list.files(f$directory, recursive = TRUE, all.files = TRUE)
  control <- new.env(parent = asNamespace("reflowR"))
  verify <- rfb_ready_only
  environment(verify) <- control
  expect_identical(body(verify), body(rfb_ready_only))
  expect_identical(formals(verify), formals(rfb_ready_only))
  calls <- 0L
  control$rfa_signature <- function(spec) {
    calls <<- calls + 1L
    if (calls == 1L) expected else list(changed = TRUE)
  }
  expect_error(verify(f$spec, f$directory, expected), "Artifact changed during verification")
  expect_identical(calls, 2L)
  expect_false(dir.exists(file.path(f$directory, ".lock")))
  expect_identical(list.files(f$directory, recursive = TRUE, all.files = TRUE), inventory)
})

test_that("postlaunch changed hash vector is refused without READY acceptance", {
  f <- bounded_control_fixture()
  source_helper <- f$control$sys.source
  f$control$sys.source <- function(file, envir, ...) {
    source_helper(file, envir, ...)
    if (basename(file) == "schema.R") {
      envir$rfx_hashes <- function(files) {
        changed <- f$pins
        changed[[1]] <- strrep("0", 64)
        changed
      }
    }
  }
  expect_error(bounded_control_run(f), "Runtime changed")
  expect_null(f$state$verified_definition)
  expect_false(file.exists(file.path(f$supervisor, "artifact-verified.rds")))
})

test_that("a correctly pinned nonexecutable file cannot be the interpreter", {
  f <- bounded_control_fixture()
  executable <- file.path(f$root, "not-executable")
  writeLines("not an executable", executable)
  Sys.chmod(executable, "0600")
  f$resources$rscript <- executable
  f$pins[[executable]] <- digest::digest(file = executable, algo = "sha256",
    serialize = FALSE)
  expect_error(bounded_control_run(f), "Executable unavailable")
  expect_identical(f$state$started, 0L)
  expect_false(dir.exists(f$supervisor))
})

test_that("nonoverlapping tracked input survives parent traversal unchanged", {
  f <- bounded_control_fixture()
  input <- file.path(f$root, "input.txt")
  writeLines("literal", input)
  f$spec$args$text <- reflow_imaging_input(input)
  before <- readBin(input, "raw", n = file.info(input)$size)
  expect_identical(bounded_control_run(f), f$state$answer)
  expect_identical(readBin(input, "raw", n = file.info(input)$size), before)
  expect_identical(f$state$request$spec, f$spec)
})

test_that("ambiguous library separator refuses before backend launch", {
  f <- bounded_control_fixture()
  f$control$.libPaths <- function() paste0("/anonymous", .Platform$path.sep, "library")
  expect_error(bounded_control_run(f), "Ambiguous library path separator")
  expect_identical(f$state$started, 0L)
})

test_that("client cleanup waits for controlled guardian exit and keeps failure", {
  f <- bounded_control_fixture()
  f$state$wait_error <- "literal READY failure"
  f$state$alive_until <- 2L
  sleeps <- numeric()
  f$control$Sys.sleep <- function(seconds) sleeps <<- c(sleeps, seconds)
  expect_error(bounded_control_run(f), "literal READY failure")
  expect_identical(sleeps, c(0.05, 0.05))
  failure <- readRDS(file.path(f$supervisor, "client-failure.rds"))
  expect_identical(failure$status, "CLIENT_FAILED")
  expect_identical(failure$error, "literal READY failure")
  expect_false(failure$guardian_alive)
  expect_false(file.exists(file.path(f$supervisor, "artifact-verified.rds")))
})
