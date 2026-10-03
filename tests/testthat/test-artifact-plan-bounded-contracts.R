# Control-plane contracts with real files; mocked transport is explicitly not
# guardian lifecycle qualification. No native writer or process is started.
bgc_fixture <- function() {
  root <- tempfile("graph-contract-")
  dir.create(root)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  control <- list(directory = file.path(root, "control"), definition = list(
    graph_directory = file.path(root, "graph"), resources = list(a = list(), b = list()),
    runtime_files = character()))
  rfbg_open(control, TRUE)
  list(root = root, control = control)
}
bgc_intent <- function(c, stage = "a", index = 1L) {
  list(stage = stage, signature = list(spec = list(literal = "anonymous")),
       dependencies = list(), supervisor = file.path(c$directory,
         sprintf("attempt-%06d", index)), resume = FALSE, retry = NULL,
       client_identity = list(pid = 123L, uid = 1000L, start_ticks = "45"),
       client_time = 1, state = "INTENT", proof = NULL, descriptor = NULL)
}
bgc_snapshot <- function(root) {
  files <- sort(list.files(root, recursive = TRUE, full.names = TRUE, all.files = TRUE))
  files <- files[!dir.exists(files)]
  stats::setNames(vapply(files, rf_file_hash, ""), substring(files, nchar(root) + 2L))
}

test_that("bounded control paths refuse aliases overlap and unavailable parents", {
  skip_if(Sys.info()[["sysname"]] != "Linux", "Linux-only canonical path contract")
  x <- bgc_fixture()
  on.exit(unlink(x$root, recursive = TRUE))
  fresh <- file.path(x$root, "fresh")
  expect_identical(rfbg_path(fresh), fresh)
  expect_identical(rfbg_path(x$control$directory, TRUE), x$control$directory)
  expect_error(rfbg_path(fresh, TRUE), "Control directory missing")
  expect_error(rfbg_path("relative"), "Canonical graph/control")
  expect_error(rfbg_path(file.path(x$root, "absent", "child")), "Canonical graph/control")
  expect_error(rfbg_path(paste0(x$root, "/./fresh")), "Canonical graph/control")
  expect_invisible(rfbg_disjoint(fresh, paste0(fresh, "-sibling")))
  expect_error(rfbg_disjoint(fresh, fresh), "paths overlap")
  expect_error(rfbg_disjoint(fresh, file.path(fresh, "child")), "paths overlap")
  expect_error(rfbg_disjoint(file.path(fresh, "child"), fresh), "paths overlap")
  expect_error(rfbg_cancel(FALSE), "callback required")
  before <- bgc_snapshot(x$root)
  expect_error(rfbg_open(x$control, TRUE), "Fresh control directory required")
  expect_invisible(rfbg_open(x$control, FALSE))
  expect_identical(bgc_snapshot(x$root), before)
})

test_that("bounded journal corruption refuses without mutation or transport", {
  x <- bgc_fixture()
  on.exit(unlink(x$root, recursive = TRUE))
  c <- x$control
  journal <- file.path(c$directory, "attempts.rds")
  calls <- 0L
  local_mocked_bindings(rfb_run = function(...) {
    calls <<- calls + 1L
    stop("unexpected transport")
  }, .package = "reflowR")
  a <- bgc_intent(c)
  false_proof <- a
  false_proof$proof <- "false"
  false_verified <- a
  false_verified$state <- "VERIFIED"
  cases <- list(
    list(value = structure(list(a), names = "a"), error = "Malformed bounded attempt journal"),
    list(value = list(list()), error = "Malformed bounded attempt record"),
    list(value = list(false_proof), error = "Unverified attempt carries completion"),
    list(value = list(false_verified), error = "Missing bounded completion proof"),
    list(value = list(bgc_intent(c, "b"), bgc_intent(c, "a", 2L)),
         error = "stage order differs"))
  for (case in cases) {
    rf_write(case$value, journal)
    before <- bgc_snapshot(x$root)
    expect_error(rfbg_control(c), case$error, fixed = TRUE)
    expect_identical(bgc_snapshot(x$root), before)
    expect_identical(calls, 0L)
  }
  rf_write(list(a), journal)
  writeLines("foreign regular file", a$supervisor)
  before <- bgc_snapshot(x$root)
  expect_error(rfbg_control(c), "Attempt directory required")
  expect_identical(bgc_snapshot(x$root), before)
  unlink(a$supervisor)
  changed <- c$definition
  changed$graph_directory <- paste0(changed$graph_directory, "-other")
  rf_write(changed, file.path(c$directory, "definition.rds"))
  before <- bgc_snapshot(x$root)
  expect_error(rfbg_control(c), "Control definition changed")
  expect_identical(bgc_snapshot(x$root), before)
})

test_that("bounded dispatch refuses orphan signatures retries and retained locks", {
  x <- bgc_fixture()
  on.exit(unlink(x$root, recursive = TRUE))
  c <- x$control
  a <- bgc_intent(c)
  node <- file.path(x$root, "node")
  calls <- 0L
  local_mocked_bindings(rfb_run = function(...) {
    calls <<- calls + 1L
    stop("unexpected transport")
  }, .package = "reflowR")
  dispatch <- function(signature = a$signature, dependencies = list(), retry = NULL) {
    rfbg_dispatch(c, "a", a$signature$spec, node, signature, dependencies,
                  retry, function() FALSE)
  }
  dir.create(node)
  before <- bgc_snapshot(x$root)
  expect_error(dispatch(), "no bounded execution intent")
  expect_identical(bgc_snapshot(x$root), before)
  rf_write(list(a), file.path(c$directory, "attempts.rds"))
  before <- bgc_snapshot(x$root)
  expect_error(dispatch(signature = list(spec = list(changed = TRUE))), "signature or lineage")
  expect_error(dispatch(dependencies = list(upstream = "changed")), "signature or lineage")
  expect_error(dispatch(), "Explicit failed-stage reconciliation")
  expect_identical(bgc_snapshot(x$root), before)
  dir.create(a$supervisor)
  dir.create(file.path(a$supervisor, "guardian.lock"))
  before <- bgc_snapshot(x$root)
  expect_error(dispatch(retry = list(reconciled_attempt = 1L, reconciliation = "inspected")),
               "Guardian lock retained")
  expect_true(dir.exists(file.path(a$supervisor, "guardian.lock")))
  expect_identical(bgc_snapshot(x$root), before)
  expect_identical(calls, 0L)
})

test_that("bounded proof refuses failure and worker lock before adopting READY", {
  x <- bgc_fixture()
  on.exit(unlink(x$root, recursive = TRUE))
  c <- x$control
  a <- bgc_intent(c)
  dir.create(a$supervisor)
  rf_write(list(error = "owned client failed"), file.path(a$supervisor, "client-failure.rds"))
  before <- bgc_snapshot(x$root)
  expect_error(rfbg_proof(c, a), "Client failure forbids READY adoption")
  expect_identical(bgc_snapshot(x$root), before)
  unlink(file.path(a$supervisor, "client-failure.rds"))
  for (name in c("request.rds", "declaration.rds", "guardian-result.rds")) {
    rf_write(list(), file.path(a$supervisor, name))
  }
  dir.create(file.path(a$supervisor, "worker"))
  dir.create(file.path(a$supervisor, "worker", "supervisor.lock"))
  local_mocked_bindings(rfb_preflight = function(...) list(), .package = "reflowR")
  before <- bgc_snapshot(x$root)
  expect_error(rfbg_proof(c, a), "Worker supervisor lock retained")
  expect_true(dir.exists(file.path(a$supervisor, "worker", "supervisor.lock")))
  expect_identical(bgc_snapshot(x$root), before)
})

test_that("bounded completion journals only a matching verified descriptor", {
  x <- bgc_fixture()
  on.exit(unlink(x$root, recursive = TRUE))
  c <- x$control
  a <- bgc_intent(c)
  a$state <- "VERIFIED"
  a$proof <- c("request.rds" = "literal-hash")
  a$descriptor <- list(directory = file.path(c$definition$graph_directory, "a"),
                       bundle = "/anonymous/payload", sha256 = "literal-payload")
  proof_calls <- 0L
  local_mocked_bindings(rfbg_proof = function(control, attempt, expected = NULL) {
    proof_calls <<- proof_calls + 1L
    expect_identical(attempt$proof, a$proof)
    if (!is.null(expected)) expect_identical(expected, a$proof)
    a$proof
  }, .package = "reflowR")
  rf_write(list(a), file.path(c$directory, "attempts.rds"))
  node <- list(definition = a$signature, dependencies = a$dependencies, descriptor = a$descriptor)
  bad <- node
  bad$descriptor$sha256 <- "changed-payload"
  before <- bgc_snapshot(x$root)
  expect_error(rfbg_completed(c, "a", bad, c$definition$graph_directory), "node differs")
  expect_identical(bgc_snapshot(x$root), before)
  expect_invisible(rfbg_completed(c, "a", node, c$definition$graph_directory))
  saved <- readRDS(file.path(c$directory, "attempts.rds"))
  expected <- a
  expected$state <- "JOURNALED"
  expect_identical(saved, list(expected))
  before <- bgc_snapshot(x$root)
  expect_invisible(rfbg_completed(c, "a", node, c$definition$graph_directory))
  expect_identical(bgc_snapshot(x$root), before)
  reused <- node
  reused$descriptor$directory <- file.path(x$root, "previous", "a")
  expect_error(rfbg_completed(c, "a", reused, c$definition$graph_directory),
               "unexplained local attempts")
  expect_gt(proof_calls, 0L)
})

test_that("bounded dispatch records intent before transport and binds returned proof", {
  x <- bgc_fixture()
  on.exit(unlink(x$root, recursive = TRUE))
  c <- x$control
  a <- bgc_intent(c)
  node <- file.path(x$root, "node")
  descriptor <- list(directory = node, bundle = file.path(node, "bundle"))
  calls <- list()
  local_mocked_bindings(
    rfb_preflight = function(...) {
      list(environment = list(
        rg_stable_identity = function(pid) list(pid = pid, uid = 1000L, start_ticks = "7")))
    },
    rfb_run = function(spec, directory, supervisor_directory, resources, runtime_files,
                       cancel, resume, reconciled_attempt, reconciliation, expected_definition) {
      records <- readRDS(file.path(c$directory, "attempts.rds"))
      expect_identical(tail(records, 1L)[[1]]$state, "INTENT")
      expect_identical(expected_definition, a$signature)
      calls[[length(calls) + 1L]] <<- list(resume = resume, reconciled = reconciled_attempt,
                                         reconciliation = reconciliation)
      descriptor
    },
    rfbg_proof = function(control, attempt, expected = NULL) c("request.rds" = "proof"),
    rfb_ready_only = function(spec, directory, signature) {
      expect_identical(signature, a$signature)
      descriptor
    }, .package = "reflowR")
  run <- function(retry = NULL) {
    rfbg_dispatch(c, "a", a$signature$spec, node,
      a$signature, list(), retry, function() FALSE)
  }
  expect_identical(run(), descriptor)
  expect_identical(calls, list(list(resume = FALSE, reconciled = NULL, reconciliation = NULL)))
  records <- readRDS(file.path(c$directory, "attempts.rds"))
  expect_identical(records[[1]]$state, "VERIFIED")
  expect_identical(records[[1]]$descriptor, descriptor)
  expect_identical(records[[1]]$proof, c("request.rds" = "proof"))
  # Explicit retry, without READY, must append its own intent and preserve prior record.
  prior <- records[[1]]
  retry <- list(reconciled_attempt = 1L, reconciliation = "externally inspected")
  expect_identical(run(retry), descriptor)
  records <- readRDS(file.path(c$directory, "attempts.rds"))
  expect_length(records, 2L)
  expect_identical(records[[1]], prior)
  expect_identical(calls[[2]]$reconciliation, "externally inspected")
  # READY adoption invokes only the separately controlled proof/READY boundary.
  dir.create(node)
  rf_write(list(marker = "anonymous"), file.path(node, "READY.rds"))
  expect_identical(run(), descriptor)
  expect_length(calls, 2L)
  expect_length(readRDS(file.path(c$directory, "attempts.rds")), 2L)
})

test_that("bounded policy preflight binds ordering paths and drift before allocation", {
  skip_if(Sys.info()[["sysname"]] != "Linux", "Linux-only preflight path contract")
  x <- bgc_fixture()
  on.exit(unlink(x$root, recursive = TRUE))
  graph <- file.path(x$root, "new-graph")
  control <- file.path(x$root, "new-control")
  # Exact graph-definition capture and backend capability are delegated here;
  # these tests exercise path/resource iteration, not package-closure validation.
  plan <- list(stages = list(a = list(declaration = list(args = list())),
                            b = list(declaration = list(args = list()))))
  resources <- list(a = list(temp_directory = file.path(x$root, "tmp"),
                             cache_directory = file.path(x$root, "cache")),
                    b = list(temp_directory = file.path(x$root, "tmp"),
                             cache_directory = file.path(x$root, "cache")))
  observed <- 0L
  drift <- FALSE
  local_mocked_bindings(
    rfg_definition = function(plan) {
      if (drift && observed > 0L) return(list(signature = "changed"))
      list(signature = "captured")
    },
    rfb_preflight = function(spec, resources, runtime_files) {
      observed <<- observed + 1L
      list(resources = resources)
    }, .package = "reflowR")
  invoke <- function(r = resources, g = graph, s = control) {
    rfbg_preflight(plan, g, s, r, character(), function() FALSE, TRUE)
  }
  before <- bgc_snapshot(x$root)
  result <- invoke()
  expect_identical(observed, 2L)
  expect_identical(result$definition$resources, resources)
  expect_identical(result$definition$graph_directory, graph)
  expect_false(file.exists(graph))
  expect_false(file.exists(control))
  expect_identical(bgc_snapshot(x$root), before)
  expect_error(invoke(resources[c("b", "a")]), "Exact ordered stage policy")
  expect_error(invoke(s = graph), "paths overlap")
  expect_error(invoke(s = x$control$directory), "Fresh graph and control")
  overlap <- resources
  overlap$a$temp_directory <- graph
  expect_error(invoke(overlap), "paths overlap")
  observed <- 0L
  drift <- TRUE
  expect_error(invoke(), "Graph changed during policy preflight")
  expect_false(file.exists(graph))
  expect_false(file.exists(control))
  expect_identical(bgc_snapshot(x$root), before)
})

# Existing independent literal receipt constructors, copied exactly except names.
bgc_literal_receipts <- function() {
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

bgc_literal_causal <- function() {
  x <- bgc_literal_receipts()
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

bgc_disk_proof <- function() {
  x <- bgc_literal_causal()
  root <- tempfile("literal-proof-")
  dir.create(root)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  x$control$directory <- file.path(root, "control")
  dir.create(x$control$directory)
  x$control$definition$graph_directory <- file.path(root, "graph")
  x$attempt$supervisor <- file.path(x$control$directory, "attempt-000001")
  dir.create(x$attempt$supervisor)
  dir.create(file.path(x$attempt$supervisor, "worker"))
  x$request$directory <- file.path(root, "graph", "a")
  saveRDS(x$request, file.path(x$attempt$supervisor, "request.rds"))
  x$request_hash <- digest::digest(file = file.path(x$attempt$supervisor, "request.rds"),
                                   algo = "sha256", serialize = FALSE)
  x$declaration$args[3:4] <- c(file.path(x$attempt$supervisor, "request.rds"), x$request_hash)
  x$declaration$directory <- x$attempt$supervisor
  x$declaration$wd <- file.path(root, "graph")
  x$worker_declaration$args[10:11] <- c(file.path(x$attempt$supervisor, "request.rds"),
                                      x$request_hash)
  x$worker_declaration$wd <- file.path(root, "graph")
  values <- list("request.rds" = x$request, "declaration.rds" = x$declaration,
    "guardian-result.rds" = x$result, "guardian-ready.rds" = x$guardian_ready,
    "worker/declaration.rds" = x$worker_declaration,
    "worker/result.rds" = x$worker_result, "worker/child.rds" = x$child)
  for (name in names(values)) saveRDS(values[[name]], file.path(x$attempt$supervisor, name))
  x$root <- root
  x$values <- values
  x$expected <- stats::setNames(vapply(names(values), function(name) {
    digest::digest(file = file.path(x$attempt$supervisor, name),
                   algo = "sha256", serialize = FALSE)
  }, ""), names(values))
  x
}

test_that("bounded proof reads all seven linked files and retains exact hashes", {
  x <- bgc_disk_proof()
  on.exit(unlink(x$root, recursive = TRUE))
  checks <- 0L
  local_mocked_bindings(rfb_preflight = function(spec, resources, runtime_files) {
    checks <<- checks + 1L
    expect_identical(spec, x$attempt$signature$spec)
    expect_identical(resources, x$control$definition$resources$a)
    expect_identical(runtime_files, x$control$definition$runtime_files)
    x$checked
  }, .package = "reflowR")
  before <- bgc_snapshot(x$root)
  expect_identical(rfbg_proof(x$control, x$attempt), x$expected)
  expect_identical(rfbg_proof(x$control, x$attempt, x$expected), x$expected)
  expect_identical(names(x$expected), c("request.rds", "declaration.rds",
    "guardian-result.rds", "guardian-ready.rds", "worker/declaration.rds",
    "worker/result.rds", "worker/child.rds"))
  expect_identical(checks, 2L)
  expect_identical(bgc_snapshot(x$root), before)
  wrong <- x$expected
  wrong[1] <- paste(rep("0", 64L), collapse = "")
  expect_error(rfbg_proof(x$control, x$attempt, wrong), "Supervisor proof changed")
  expect_identical(bgc_snapshot(x$root), before)
})

test_that("bounded disk proof refuses substituted and missing evidence without repair", {
  x <- bgc_disk_proof()
  on.exit(unlink(x$root, recursive = TRUE))
  local_mocked_bindings(rfb_preflight = function(...) x$checked, .package = "reflowR")
  mutations <- list(
    "request.rds" = list(field = "directory", value = "/foreign/graph",
      error = "Supervisor request differs"),
    "declaration.rds" = list(field = "client_time", value = 99,
      error = "Supervisor declaration differs"),
    "guardian-result.rds" = list(field = "client_alive", value = FALSE,
      error = "Successful guardian cleanup"),
    "guardian-ready.rds" = list(field = "pid", value = 999L, error = "Guardian readiness identity"),
    "worker/declaration.rds" = list(field = "wd", value = "/foreign",
      error = "Worker dispatch differs"),
    "worker/result.rds" = list(field = "elapsed_seconds", value = 99,
      error = "Worker result differs"),
    "worker/child.rds" = list(field = "supervisor_pid", value = 999L,
      error = "Worker child supervisor identity"))
  for (name in names(mutations)) {
    change <- mutations[[name]]
    value <- x$values[[name]]
    value[[change$field]] <- change$value
    path <- file.path(x$attempt$supervisor, name)
    saveRDS(value, path)
    before <- bgc_snapshot(x$root)
    expect_error(rfbg_proof(x$control, x$attempt), change$error, fixed = TRUE)
    expect_identical(bgc_snapshot(x$root), before)
    saveRDS(x$values[[name]], path)
    unlink(path)
    before <- bgc_snapshot(x$root)
    expect_error(rfbg_proof(x$control, x$attempt))
    expect_false(file.exists(path))
    expect_identical(bgc_snapshot(x$root), before)
    saveRDS(x$values[[name]], path)
  }
  expect_identical(rfbg_proof(x$control, x$attempt), x$expected)
})

test_that("causal proof refuses malformed channel declaration environment and owner", {
  x <- bgc_literal_causal()
  changed <- x
  changed$declaration <- structure(changed$declaration, class = "foreign")
  expect_error(do.call(rfbg_causal_fields, changed), "Malformed supervisor declaration")
  changed <- x
  changed$guardian_ready$fd$control$flags <- "02"
  expect_error(do.call(rfbg_causal_fields, changed), "Guardian channel flags differ")
  changed <- x
  changed$worker_declaration$environment <- unname(changed$worker_declaration$environment)
  expect_error(do.call(rfbg_causal_fields, changed), "Worker environment schema differs")
  changed <- x
  changed$worker_declaration$environment[["RFX_EXECUTOR_OWNER"]] <- "not-a-token"
  expect_error(do.call(rfbg_causal_fields, changed), "Worker ownership token invalid")
  expect_invisible(do.call(rfbg_causal_fields, x))
})

test_that("control creation failure leaves no definition or journal", {
  root <- tempfile("control-create-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  path <- file.path(root, "control")
  control <- list(directory = path, definition = list(literal = "policy"))
  original <- rfbg_open
  e <- new.env(parent = environment(original))
  # A local filesystem failure boundary, with exact executable function preserved.
  e$dir.create <- function(...) FALSE
  controlled <- original
  environment(controlled) <- e
  expect_identical(body(controlled), body(original))
  expect_identical(formals(controlled), formals(original))
  expect_error(controlled(control, TRUE), "Fresh control directory required")
  expect_false(file.exists(path))
  expect_length(list.files(root, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("bounded graph public wrappers forward exact run and resume contracts", {
  captured <- list()
  sentinel <- list(literal = "delegated-descriptor")
  local_mocked_bindings(rfbg_execute = function(...) {
    captured[[length(captured) + 1L]] <<- list(...)
    sentinel
  }, .package = "reflowR")
  plan <- list(literal = "plan")
  resources <- list(a = list(timeout_seconds = 7))
  runtime <- c("/anonymous/runtime" = "hash")
  cancel <- function() FALSE
  prior <- list(literal = "prior")
  retry <- list(a = list(reconciled_attempt = 1L, reconciliation = "inspected"))
  expect_identical(reflow_artifact_plan_run_bounded(plan, "/graph", "/control",
    resources, runtime, previous = prior, cancel = cancel), sentinel)
  expect_identical(captured[[1]], list(plan, "/graph", prior, TRUE, list(),
    "/control", resources, runtime, cancel))
  expect_identical(reflow_artifact_plan_resume_bounded(plan, "/graph", "/control",
    resources, runtime, cancel = cancel, reconciliations = retry), sentinel)
  expect_identical(captured[[2]], list(plan, "/graph", NULL, FALSE, retry,
    "/control", resources, runtime, cancel))
  expect_length(captured, 2L)
})

test_that("bounded preflight keeps tracked input files separate and untouched", {
  skip_if(Sys.info()[["sysname"]] != "Linux", "Linux-only preflight path contract")
  x <- bgc_fixture()
  on.exit(unlink(x$root, recursive = TRUE))
  tracked <- file.path(x$root, "tracked")
  dir.create(tracked)
  input <- file.path(tracked, "input.txt")
  writeLines("literal tracked input", input)
  reference <- reflow_imaging_input(input)
  plan <- list(stages = list(a = list(declaration = list(args = list(input = reference)))))
  resources <- list(a = list(temp_directory = file.path(x$root, "tmp"),
                             cache_directory = file.path(x$root, "cache")))
  checked <- 0L
  local_mocked_bindings(
    rfg_definition = function(plan) list(signature = "captured"),
    rfb_preflight = function(spec, resources, runtime_files) {
      checked <<- checked + 1L
      expect_identical(spec$args$input, reference)
      list(resources = resources)
    }, .package = "reflowR")
  graph <- file.path(x$root, "new-graph")
  control <- file.path(x$root, "new-control")
  before <- bgc_snapshot(x$root)
  result <- rfbg_preflight(plan, graph, control, resources, character(),
                           function() FALSE, TRUE)
  expect_identical(result$definition$graph_directory, graph)
  expect_identical(checked, 1L)
  expect_false(file.exists(graph))
  expect_false(file.exists(control))
  expect_identical(bgc_snapshot(x$root), before)
  # Existing tracked file collides only after fresh-path check when resuming:
  # use its containing existing directory as graph and a distinct existing control.
  expect_error(rfbg_preflight(plan, tracked, x$control$directory, resources,
    character(), function() FALSE, FALSE), "paths overlap")
  expect_identical(bgc_snapshot(x$root), before)
})

test_that("valid bounded alignment does not rewrite attempts", {
  x <- bgc_fixture()
  on.exit(unlink(x$root, recursive = TRUE))
  control <- x$control
  journal <- file.path(control$directory, "attempts.rds")
  before <- bgc_snapshot(x$root)
  expect_invisible(rfbg_alignment(control, list()))
  expect_identical(bgc_snapshot(x$root), before)
  rf_write(list(bgc_intent(control, "a")), journal)
  before <- bgc_snapshot(x$root)
  expect_invisible(rfbg_alignment(control, list()))
  expect_identical(bgc_snapshot(x$root), before)
  rf_write(list(bgc_intent(control, "b")), journal)
  before <- bgc_snapshot(x$root)
  expect_invisible(rfbg_alignment(control, list(a = list(literal = "completed"))))
  expect_identical(bgc_snapshot(x$root), before)
})

test_that("causal liveness refuses invalid elapsed values with all fields retained", {
  original <- bgc_literal_causal()
  for (elapsed in list(-1, NA_real_, Inf, "1", c(0, 1))) {
    changed <- original
    changed$result$liveness_observations[[1]]$elapsed <- elapsed
    expect_identical(names(changed$result$liveness_observations[[1]]),
                     names(original$result$liveness_observations[[1]]))
    expect_identical(changed$result$liveness_observations[[1]][-6],
                     original$result$liveness_observations[[1]][-6])
    expect_error(do.call(rfbg_causal_fields, changed), "Client liveness evidence differs")
  }
  expect_invisible(do.call(rfbg_causal_fields, original))
})
