bounded_graph_control_fixture <- function() {
  root <- tempfile("bounded-graph-control-")
  dir.create(root)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  list(directory = root, definition = list(
    schema = "reflow_bounded_graph_control_1",
    graph_directory = file.path(dirname(root), "anonymous-graph"),
    resources = list(a = list(), b = list()), runtime_files = character(),
    libraries = character(), backend = "linux_guardian_1", graph_definition = NULL
  ))
}

bounded_graph_intent_fixture <- function(control, stage = "a") {
  list(stage = stage, signature = list(spec = list()), dependencies = list(),
       supervisor = file.path(control$directory, "attempt-000001"),
       resume = FALSE, retry = NULL,
       client_identity = list(pid = 123L, uid = 1000L, start_ticks = "456"),
       client_time = 1, state = "INTENT", proof = NULL,
       descriptor = NULL)
}

test_that("bounded graph cancellation is literal and fail closed", {
  expect_invisible(rfbg_cancel(function() FALSE))
  expect_error(rfbg_cancel(function() TRUE), "Graph cancelled")
  expect_error(rfbg_cancel(function() NA), "Invalid cancellation result")
  expect_error(rfbg_cancel(function() c(FALSE, FALSE)), "Invalid cancellation result")
  expect_error(rfbg_cancel(function() 0), "Invalid cancellation result")
  expect_error(rfbg_cancel(function() stop("callback failed")), "callback failed")
})

test_that("control journals are required and do not reconstruct from attempts", {
  c <- bounded_graph_control_fixture()
  on.exit(unlink(c$directory, recursive = TRUE))
  rf_write(c$definition, file.path(c$directory, "definition.rds"))
  expect_error(rfbg_control(c))
  expect_false(file.exists(file.path(c$directory, "attempts.rds")))
  rf_write(list(), file.path(c$directory, "attempts.rds"))
  expect_identical(rfbg_control(c), list())
  before <- rf_file_hash(file.path(c$directory, "attempts.rds"))
  dir.create(file.path(c$directory, "attempt-000001"))
  expect_error(rfbg_control(c), "Unexpected bounded control inventory")
  expect_identical(rf_file_hash(file.path(c$directory, "attempts.rds")), before)
})

test_that("control policy mutation refuses without journal mutation", {
  c <- bounded_graph_control_fixture()
  on.exit(unlink(c$directory, recursive = TRUE))
  rf_write(c$definition, file.path(c$directory, "definition.rds"))
  rf_write(list(), file.path(c$directory, "attempts.rds"))
  before <- rf_file_hash(file.path(c$directory, "attempts.rds"))
  changed <- c
  changed$definition$backend <- "different-backend"
  expect_error(rfbg_open(changed, FALSE), "policy or definition changed")
  expect_identical(rf_file_hash(file.path(c$directory, "attempts.rds")), before)
})

test_that("attempts beyond the first pending node refuse", {
  c <- bounded_graph_control_fixture()
  on.exit(unlink(c$directory, recursive = TRUE))
  rf_write(c$definition, file.path(c$directory, "definition.rds"))
  rf_write(list(bounded_graph_intent_fixture(c, "b")),
           file.path(c$directory, "attempts.rds"))
  expect_error(rfbg_alignment(c, list()), "beyond pending node")
  expect_length(rfbg_control(c), 1L)
})

test_that("local completed nodes require bounded provenance but reuse does not", {
  c <- bounded_graph_control_fixture()
  on.exit(unlink(c$directory, recursive = TRUE))
  rf_write(c$definition, file.path(c$directory, "definition.rds"))
  rf_write(list(), file.path(c$directory, "attempts.rds"))
  node <- list(descriptor = list(directory = file.path(c$definition$graph_directory, "a")))
  expect_error(rfbg_completed(c, "a", node, c$definition$graph_directory),
               "lacks bounded provenance")
  node$descriptor$directory <- file.path(dirname(c$directory), "previous", "a")
  expect_invisible(rfbg_completed(c, "a", node, c$definition$graph_directory))
  expect_identical(rfbg_control(c), list())
})

test_that("guardian locks cannot be removed by recovery", {
  c <- bounded_graph_control_fixture()
  on.exit(unlink(c$directory, recursive = TRUE))
  attempt <- bounded_graph_intent_fixture(c)
  dir.create(attempt$supervisor)
  dir.create(file.path(attempt$supervisor, "guardian.lock"))
  expect_error(rfbg_proof(c, attempt), "Guardian lock retained")
  expect_true(dir.exists(file.path(attempt$supervisor, "guardian.lock")))
})

test_that("bounded dispatch accepts only the graph captured definition", {
  local_mocked_bindings(rfb_capability = function() invisible(TRUE),
                       rfa_signature = function(spec) list(signature = "current"),
                       rfb_preflight = function(...) stop("must not reach preflight"),
                       .package = "reflowR")
  expect_error(rfb_run(list(), "unused", "unused", list(), character(),
                       function() FALSE, FALSE, NULL, NULL,
                       expected_definition = list(signature = "old")),
               "Captured artifact definition changed")
})

# Literal anonymous receipt. No production constructor builds the expected
# request or declaration, and no process is started by these predicates.
bounded_graph_receipt_fixture <- function() {
  runtime <- c("/runtime/file" = paste(rep("a", 64L), collapse = ""))
  resources <- list(timeout_seconds = 10, address_space_bytes = 1024,
                    nice = 10, threads = 2, temp_directory = "/tmp-local",
                    cache_directory = "/cache-local", rscript = "/Rscript",
                    prlimit = "/prlimit", nice_command = "/nice")
  identity <- list(pid = 123L, uid = 1000L, start_ticks = "456")
  observation <- list(locale = "C", rng_kind = c("Mersenne-Twister", "Inversion", "Rejection"))
  signature <- list(spec = list(literal = "writer"), base = list(runtime_hash = "runtime"))
  control <- list(definition = list(graph_directory = "/graph",
    resources = list(a = resources), runtime_files = runtime,
    libraries = "/library", runtime_observation = observation))
  attempt <- list(stage = "a", signature = signature, supervisor = "/control/attempt-000001",
    resume = FALSE, retry = NULL, client_identity = identity, client_time = 1)
  checked <- list(package_path = "/library/reflowR", packages = c(reflowR = "/library/reflowR"),
    files = paste0("/backend/", c("executor.R", "channel.R", "client.R",
                                "guardian.R", "worker.R", "schema.R")))
  request <- list(schema = 1L, spec = list(literal = "writer"), expected_definition = signature,
    directory = "/graph/a", resume = FALSE, reconciled_attempt = NULL, reconciliation = NULL,
    libraries = "/library", package_path = "/library/reflowR",
    package_paths = c(reflowR = "/library/reflowR"), runtime_sha256 = runtime,
    runtime_observation = observation, channel_script = "/backend/channel.R")
  declaration <- list(mode = "worker", libraries = "/library",
    package_paths = c(reflowR = "/library/reflowR"), executor_script = "/backend/executor.R",
    command = "/Rscript", args = c("--vanilla", "/backend/worker.R",
      "/control/attempt-000001/request.rds", "request-hash"),
    wd = "/graph", runtime_sha256 = runtime, all_source_sha256 = runtime,
    temp_directory = "/tmp-local", cache_directory = "/cache-local",
    deadline_seconds = 10, address_space_bytes = 1024, nice = 10, threads = 2,
    environment = c(R_LIBS = "/library"), prlimit = "/prlimit", nice_command = "/nice",
    client = identity, client_time = 1, token = paste(rep("b", 64L), collapse = ""),
    directory = "/control/attempt-000001", client_writer_inode = "101",
    control_read_inode = "102", status_write_inode = "103",
    channel_script = "/backend/channel.R", guardian_script = "/backend/guardian.R",
    source_sha256 = runtime)
  execution <- list(schema = 1L, status = "EXECUTION_SUCCEEDED_UNVERIFIED",
    diagnostic = NULL, cleanup_error = NULL, post_pin_error = NULL,
    exit_code = 0L, killed_pids = integer(), elapsed_seconds = 1,
    artifact_verified = FALSE, generation_selector_invoked = FALSE,
    owned_observation = list(owned = list(), uncertain = list(), excluded_preexisting = list()),
    cleanup_scope = "anonymous literal", runtime_pin_scope = "anonymous literal")
  result <- list(schema = 1L, status = "EXECUTION_SUCCEEDED_UNVERIFIED", reason = NULL,
    execution = execution, artifact_verified = FALSE, worker_started = TRUE, client_alive = TRUE,
    guardian_identity = list(pid = 124L, uid = 1000L, start_ticks = "457"),
    liveness_observations = list())
  list(control = control, attempt = attempt, checked = checked, request = request,
       declaration = declaration, result = result, request_hash = "request-hash")
}

test_that("complete intended receipt accepts only literal successful execution", {
  fixture <- bounded_graph_receipt_fixture()
  expect_invisible(do.call(rfbg_proof_fields, fixture))
  for (name in names(fixture$request)) {
    changed <- fixture
    changed$request[[name]] <- NULL
    expect_error(do.call(rfbg_proof_fields, changed), "Supervisor request differs")
  }
  for (name in names(fixture$declaration)) {
    changed <- fixture
    changed$declaration[[name]] <- NULL
    expect_error(do.call(rfbg_proof_fields, changed), "[Ss]upervisor")
  }
  for (name in names(fixture$result$execution)) {
    changed <- fixture
    changed$result$execution[[name]] <- NULL
    expect_error(do.call(rfbg_proof_fields, changed), "Successful guardian cleanup")
  }
})

test_that("status resource runtime and request substitutions refuse orphan adoption", {
  fixture <- bounded_graph_receipt_fixture()
  for (status in c("CHILD_FAILED", "CLIENT_LOST", "FINAL_PIN_FAILURE")) {
    changed <- fixture
    changed$result$execution$status <- status
    expect_error(do.call(rfbg_proof_fields, changed), "Successful guardian cleanup")
    changed <- fixture
    changed$result$status <- status
    expect_error(do.call(rfbg_proof_fields, changed), "Successful guardian cleanup")
  }
  for (code in list(1L, 0, NA_integer_)) {
    changed <- fixture
    changed$result$execution$exit_code <- code
    expect_error(do.call(rfbg_proof_fields, changed), "Successful guardian cleanup")
  }
  for (key in c("wd", "environment", "source_sha256", "command", "args",
                "runtime_sha256", "all_source_sha256", "client", "client_time")) {
    changed <- fixture
    changed$declaration[[key]] <- "changed"
    expect_error(do.call(rfbg_proof_fields, changed), "Supervisor declaration differs")
  }
  for (key in c("schema", "runtime_observation", "expected_definition", "runtime_sha256")) {
    changed <- fixture
    changed$request[[key]] <- "changed"
    expect_error(do.call(rfbg_proof_fields, changed), "Supervisor request differs")
  }
  changed <- fixture
  changed$request_hash <- "different-request-hash"
  expect_error(do.call(rfbg_proof_fields, changed), "Supervisor declaration differs")
  changed <- fixture
  changed$result$execution$owned_observation$uncertain <- list("unknown")
  expect_error(do.call(rfbg_proof_fields, changed), "Successful guardian cleanup")
  changed <- fixture
  changed$result$execution$cleanup_error <- "unresolved"
  expect_error(do.call(rfbg_proof_fields, changed), "Successful guardian cleanup")
})

bounded_graph_causal_fixture <- function() {
  x <- bounded_graph_receipt_fixture()
  x$guardian_ready <- list(pid = 124L,
    identity = list(pid = 124L, uid = 1000L, start_ticks = "457"),
    fd = list(control = list(fd = 3L, flags = "02004002", inode = "102"),
              status = list(fd = 4L, flags = "02004002", inode = "103")),
    client_writer_absent = TRUE, worker_token_absent = TRUE)
  x$result$liveness_observations <- list(list(alive = TRUE,
    first = list(identity = list(pid = 123L, uid = 1000L, start_ticks = "456"), state = "S"),
    last = list(identity = list(pid = 123L, uid = 1000L, start_ticks = "456"), state = "S"),
    handle_running = TRUE, error = NULL, elapsed = 1))
  x$worker_declaration <- list(schema = 1L, command = "/prlimit",
    args = c("--as=1024", "--core=0", "--", "/nice", "-n", "0", "/Rscript",
      "--vanilla", "/backend/worker.R", "/control/attempt-000001/request.rds", "request-hash"),
    wd = "/graph", environment = c("current", R_LIBS = "/library",
      RFX_FORBIDDEN_FD_INODES = "101,102,103",
      RFX_EXECUTOR_OWNER = paste(rep("c", 64L), collapse = ""),
      TMPDIR = "/tmp-local", TMP = "/tmp-local", TEMP = "/tmp-local",
      XDG_CACHE_HOME = "/cache-local", OPENBLAS_NUM_THREADS = "2",
      OMP_NUM_THREADS = "2", MKL_NUM_THREADS = "2"),
    runtime_sha256 = c("/runtime/file" = paste(rep("a", 64L), collapse = "")),
    process_baseline = list(identities = list(), unknown = character(),
                            boot_id = "anonymous-boot", observed_at = "anonymous-time"),
    limits = list(timeout_seconds = 9, poll_seconds = 0.1, wait_seconds = 5,
      address_space_bytes = 1024, nice = 10,
      nice_plan = list(inherited = 10L, requested = 10, increment = 0),
      threads = 2, core_bytes = 0),
    capabilities = list(linux = TRUE, processx = TRUE, ps = TRUE, digest = TRUE),
    artifact_verified = FALSE)
  x$worker_result <- x$result$execution
  x$child <- list(pid = 125L, supervisor_pid = 124L, start_time = 2)
  x
}

test_that("orphan proof binds guardian channel client and actual worker receipts", {
  x <- bounded_graph_causal_fixture()
  expect_invisible(do.call(rfbg_causal_fields, x))
  # A generic successful result from another guardian is not this attempt.
  foreign <- x$result
  foreign$guardian_identity <- list(pid = 224L, uid = 1000L, start_ticks = "557")
  changed <- x
  changed$result <- foreign
  expect_error(do.call(rfbg_causal_fields, changed), "Guardian readiness identity")
  changed <- x
  changed$guardian_ready$identity <- foreign$guardian_identity
  changed$guardian_ready$pid <- 224L
  expect_error(do.call(rfbg_causal_fields, changed), "Guardian readiness identity")
  # Even a matched foreign ready/result pair lacks this worker's supervisor.
  changed$result <- foreign
  expect_error(do.call(rfbg_causal_fields, changed), "Worker child supervisor identity")
  changed <- x
  changed$guardian_ready$fd$control$inode <- "902"
  expect_error(do.call(rfbg_causal_fields, changed), "Guardian channel receipt")
  changed <- x
  changed$result$liveness_observations[[1]]$last$identity$start_ticks <- "foreign"
  expect_error(do.call(rfbg_causal_fields, changed), "Client liveness identity")
  changed <- x
  changed$result$liveness_observations <- list()
  expect_error(do.call(rfbg_causal_fields, changed), "Client liveness evidence missing")
  changed <- x
  changed$child$supervisor_pid <- 224L
  expect_error(do.call(rfbg_causal_fields, changed), "Worker child supervisor identity")
  changed <- x
  changed$worker_result$elapsed_seconds <- 999
  expect_error(do.call(rfbg_causal_fields, changed), "Worker result differs")
  changed <- x
  changed$worker_declaration$args[10] <- "/foreign/request.rds"
  expect_error(do.call(rfbg_causal_fields, changed), "Worker dispatch differs")
  changed <- x
  changed$worker_declaration$args[11] <- "foreign-request-hash"
  expect_error(do.call(rfbg_causal_fields, changed), "Worker dispatch differs")
  changed <- x
  changed$worker_declaration$environment[["RFX_FORBIDDEN_FD_INODES"]] <- "901,902,903"
  expect_error(do.call(rfbg_causal_fields, changed), "Worker dispatch differs")
})

test_that("late worker receipt omissions and resource substitutions fail closed", {
  x <- bounded_graph_causal_fixture()
  for (name in names(x$worker_declaration)) {
    changed <- x
    changed$worker_declaration[[name]] <- NULL
    expect_error(do.call(rfbg_causal_fields, changed), "Worker declaration schema")
  }
  for (name in names(x$child)) {
    changed <- x
    changed$child[[name]] <- NULL
    expect_error(do.call(rfbg_causal_fields, changed), "Worker child supervisor identity")
  }
  changed <- x
  changed$worker_declaration$limits$timeout_seconds <- 11
  expect_error(do.call(rfbg_causal_fields, changed), "Worker resource limits")
  changed <- x
  changed$worker_declaration$limits$nice_plan$increment <- 1
  expect_error(do.call(rfbg_causal_fields, changed), "Worker resource limits")
  changed <- x
  changed$worker_declaration$environment[["OMP_NUM_THREADS"]] <- "1"
  expect_error(do.call(rfbg_causal_fields, changed), "Worker dispatch differs")
  changed <- x
  changed$guardian_ready$fd$status$flags <- "04002"
  expect_error(do.call(rfbg_causal_fields, changed), "Guardian channel flags")
  changed <- x
  changed$result$liveness_observations[[1]]$first$state <- "Z"
  expect_error(do.call(rfbg_causal_fields, changed), "Client liveness identity")
})
