# Scheduler contract tests. Process execution and proof validation are delegated
# controlled boundaries; real journal writes, generation locks and paths remain.
bounded_execute_fixture <- function() {
  root <- tempfile("bounded-execute-")
  dir.create(root)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  state <- new.env(parent = emptyenv())
  state$events <- character()
  state$fail <- NULL
  state$definition_calls <- 0L
  state$drift <- FALSE
  state$old <- list()
  state$signature_override <- NULL
  state$dependencies <- list()
  state$proof_fail <- FALSE
  state$run_count <- 0L
  record <- function(x) state$events <- c(state$events, x)
  e <- new.env(parent = environment(rfbg_execute))
  execute <- rfbg_execute
  environment(execute) <- e
  stopifnot(identical(body(execute), body(rfbg_execute)),
            identical(formals(execute), formals(rfbg_execute)))
  plan <- list(stages = list(
    producer = list(id = "producer", declaration = list(args = list())),
    consumer = list(id = "consumer", declaration = list(args = list()))
  ))
  graph <- file.path(root, "graph")
  control <- file.path(root, "control")
  e$rfbg_preflight <- function(...) {
    record("preflight")
    list(directory = control, definition = list(policy = "literal"))
  }
  e$rfg_definition <- function(plan) {
    state$definition_calls <- state$definition_calls + 1L
    list(stages = names(plan$stages), revision =
      if (state$drift && state$definition_calls > 1L) 2L else 1L)
  }
  e$rfg_resolve <- function(stage, nodes) {
    if (stage$id == "consumer") stopifnot(!is.null(nodes$producer))
    record(paste0("resolve:", stage$id))
    stage
  }
  e$rfa_signature <- function(spec) {
    list(id = spec$id, revision = if (identical(state$signature_override,
      spec$id)) 2L else 1L)
  }
  e$rfg_dependencies <- function(plan, id, nodes) {
    if (id == "producer") list() else
      list(producer = nodes$producer$definition, extra = state$dependencies)
  }
  e$rfg_records <- function(directory, definition, ...) {
    record("records")
    readRDS(file.path(directory, "nodes.rds"))
  }
  e$rfbg_control <- function(control) record("control")
  e$rfbg_alignment <- function(control, nodes) record("alignment")
  e$rfbg_completed <- function(control, id, node, directory) {
    # Completion/proof boundary must follow durable node journaling.
    stopifnot(identical(readRDS(file.path(directory, "nodes.rds"))[[id]], node))
    record(paste0("completed:", id))
    if (state$proof_fail) stop("controlled proof refusal")
  }
  e$rfb_ready_only <- function(spec, directory, signature) {
    record(paste0("ready:", spec$id))
    stopifnot(file.exists(file.path(directory, "payload")))
    list(directory = directory, id = spec$id)
  }
  e$rfbg_dispatch <- function(control, id, spec, node_path, signature,
                             dependencies, reconciliation, cancel) {
    record(paste0("dispatch:", id))
    state$run_count <- state$run_count + 1L
    if (identical(state$fail, id)) stop("controlled writer failure")
    if (!dir.exists(node_path)) dir.create(node_path)
    writeLines(id, file.path(node_path, "payload"))
    list(directory = node_path, id = id)
  }
  e$rfg_history <- function(previous, ...) {
    record("history")
    list(nodes = state$old, binding = list(directory = previous,
      ready_hash = "literal-history"), locks = character())
  }
  run <- function(fresh = TRUE, previous = NULL, reconciliations = list(),
                  cancel = function() FALSE) {
    execute(plan, graph, previous, fresh, reconciliations,
            control, list(), character(), cancel)
  }
  list(root = root, graph = graph, control = control, state = state,
       env = e, plan = plan, run = run)
}

bounded_execute_unlocked <- function(f) {
  expect_false(dir.exists(file.path(f$graph, ".lock")))
  expect_false(dir.exists(file.path(f$control, ".lock")))
}

test_that("bounded scheduler journals producer before consumer and verifies cache", {
  f <- bounded_execute_fixture()
  on.exit(unlink(f$root, recursive = TRUE))
  result <- f$run()
  expect_identical(names(result$nodes), c("producer", "consumer"))
  expect_identical(f$state$run_count, 2L)
  expect_true(file.exists(file.path(f$graph, "READY.rds")))
  events <- f$state$events
  expect_lt(match("completed:producer", events), match("dispatch:consumer", events))
  before <- readRDS(file.path(f$graph, "nodes.rds"))
  f$state$events <- character()
  cached <- f$run(FALSE)
  expect_identical(cached, result)
  expect_identical(readRDS(file.path(f$graph, "nodes.rds")), before)
  expect_identical(f$state$run_count, 2L)
  expect_identical(f$state$events[startsWith(f$state$events, "ready:")],
                   c("ready:producer", "ready:consumer"))
  bounded_execute_unlocked(f)
})

test_that("failed dispatch leaves a durable prefix and resume dispatches only missing node", {
  f <- bounded_execute_fixture()
  on.exit(unlink(f$root, recursive = TRUE))
  f$state$fail <- "consumer"
  expect_error(f$run(), "controlled writer failure")
  prefix <- readRDS(file.path(f$graph, "nodes.rds"))
  expect_identical(names(prefix), "producer")
  expect_false(file.exists(file.path(f$graph, "READY.rds")))
  bounded_execute_unlocked(f)
  f$state$fail <- NULL
  f$state$events <- character()
  f$run(FALSE)
  expect_identical(f$state$events[startsWith(f$state$events, "dispatch:")],
                   "dispatch:consumer")
  expect_identical(readRDS(file.path(f$graph, "nodes.rds"))$producer, prefix$producer)
  bounded_execute_unlocked(f)
})

test_that("completed-node drift and proof refusal stop before further dispatch", {
  f <- bounded_execute_fixture()
  on.exit(unlink(f$root, recursive = TRUE))
  f$state$fail <- "consumer"
  expect_error(f$run(), "controlled writer failure")
  before <- readRDS(file.path(f$graph, "nodes.rds"))
  count <- f$state$run_count
  f$state$signature_override <- "producer"
  expect_error(f$run(FALSE), "Completed node inputs or runtime changed")
  f$state$signature_override <- NULL
  f$state$proof_fail <- TRUE
  expect_error(f$run(FALSE), "controlled proof refusal")
  expect_identical(f$state$run_count, count)
  expect_identical(readRDS(file.path(f$graph, "nodes.rds")), before)
  bounded_execute_unlocked(f)
})

test_that("boundary cancellation retains accepted prefix without dispatching consumer", {
  f <- bounded_execute_fixture()
  on.exit(unlink(f$root, recursive = TRUE))
  cancel <- function() f$state$run_count == 1L
  expect_error(f$run(cancel = cancel), "Graph cancelled")
  expect_identical(f$state$run_count, 1L)
  expect_identical(names(readRDS(file.path(f$graph, "nodes.rds"))), "producer")
  expect_false(dir.exists(file.path(f$graph, "consumer")))
  expect_false(file.exists(file.path(f$graph, "READY.rds")))
  bounded_execute_unlocked(f)
})

test_that("final declaration drift never publishes READY", {
  f <- bounded_execute_fixture()
  on.exit(unlink(f$root, recursive = TRUE))
  f$state$drift <- TRUE
  expect_error(f$run(), "Graph changed during run")
  expect_identical(names(readRDS(file.path(f$graph, "nodes.rds"))),
                   c("producer", "consumer"))
  expect_false(file.exists(file.path(f$graph, "READY.rds")))
  bounded_execute_unlocked(f)
})

test_that("malformed reconciliations refuse before generation allocation", {
  bad <- list(1, structure(list(), class = "foreign"), list(unknown = list()),
              list(producer = list(reconciliation = "only")))
  for (x in bad) {
    f <- bounded_execute_fixture()
    expect_error(f$run(reconciliations = x), "Invalid .*reconciliation")
    expect_false(dir.exists(f$graph))
    expect_false(dir.exists(f$control))
    unlink(f$root, recursive = TRUE)
  }
})

test_that("resume refuses changed declaration and missing generation without writes", {
  f <- bounded_execute_fixture()
  on.exit(unlink(f$root, recursive = TRUE))
  expect_error(f$run(FALSE), "Generation directory missing")
  f$run()
  before <- readRDS(file.path(f$graph, "definition.rds"))
  f$state$drift <- TRUE
  expect_error(f$run(FALSE), "Graph definition, inputs or runtime changed")
  expect_identical(readRDS(file.path(f$graph, "definition.rds")), before)
  bounded_execute_unlocked(f)
})

test_that("previous generation reuse is in place and lineage change dispatches consumer", {
  old <- bounded_execute_fixture()
  f <- bounded_execute_fixture()
  on.exit(unlink(c(old$root, f$root), recursive = TRUE))
  prior <- old$run()
  f$state$old <- prior$nodes
  f$state$dependencies <- list(changed = TRUE)
  result <- f$run(previous = old$graph)
  expect_identical(result$nodes$producer, prior$nodes$producer)
  expect_false(dir.exists(file.path(f$graph, "producer")))
  expect_identical(f$state$events[startsWith(f$state$events, "dispatch:")],
                   "dispatch:consumer")
  expect_identical(readLines(file.path(old$graph, "producer", "payload")),
                   "producer")
  f$state$events <- character()
  f$run(FALSE)
  expect_true("history" %in% f$state$events)
  expect_false(any(startsWith(f$state$events, "dispatch:")))
  bounded_execute_unlocked(f)
})

test_that("matching prior nodes reuse without allocating local node directories", {
  old <- bounded_execute_fixture()
  f <- bounded_execute_fixture()
  on.exit(unlink(c(old$root, f$root), recursive = TRUE))
  prior <- old$run()
  f$state$old <- prior$nodes
  result <- f$run(previous = old$graph)
  expect_identical(result$nodes, prior$nodes)
  expect_identical(f$state$run_count, 0L)
  expect_false(dir.exists(file.path(f$graph, "producer")))
  expect_false(dir.exists(file.path(f$graph, "consumer")))
  expect_true(file.exists(file.path(f$graph, "READY.rds")))
  bounded_execute_unlocked(f)
})

test_that("interrupted dispatch recovery journals returned descriptor without rerunning writer", {
  f <- bounded_execute_fixture()
  on.exit(unlink(f$root, recursive = TRUE))
  original <- f$env$rfbg_dispatch
  interrupted <- FALSE
  f$env$rfbg_dispatch <- function(control, id, spec, node_path, signature,
                                  dependencies, reconciliation, cancel) {
    if (id == "consumer" && interrupted) {
      # Controlled recovery boundary stands for separately tested causal proof.
      stopifnot(identical(readLines(file.path(node_path, "payload")), id))
      return(list(directory = node_path, id = id))
    }
    descriptor <- original(control, id, spec, node_path, signature,
                           dependencies, reconciliation, cancel)
    if (id == "consumer") {
      interrupted <<- TRUE
      stop("controlled post-writer interruption")
    }
    descriptor
  }
  expect_error(f$run(), "controlled post-writer interruption")
  expect_identical(names(readRDS(file.path(f$graph, "nodes.rds"))), "producer")
  expect_identical(f$state$run_count, 2L)
  expect_false(file.exists(file.path(f$graph, "READY.rds")))
  result <- f$run(FALSE)
  expect_identical(f$state$run_count, 2L)
  expect_identical(names(result$nodes), c("producer", "consumer"))
  expect_true(file.exists(file.path(f$graph, "READY.rds")))
  bounded_execute_unlocked(f)
})

# Residual scheduler refusals: exact scheduler clone, real filesystem changes.
bounded_execute_at <- function(f, directory = f$graph, plan = f$plan,
                               previous = NULL, fresh = TRUE,
                               reconciliations = list()) {
  execute <- get("execute", envir = environment(f$run), inherits = FALSE)
  execute(plan, directory, previous, fresh, reconciliations,
          f$control, list(), character(), function() FALSE)
}

test_that("missing destination parent refuses before allocation or dispatch", {
  f <- bounded_execute_fixture()
  on.exit(unlink(f$root, recursive = TRUE))
  absent <- file.path(f$root, "missing-parent", "generation")
  expect_error(bounded_execute_at(f, absent), "Destination parent must exist")
  expect_false(dir.exists(dirname(absent)))
  expect_false(dir.exists(f$control))
  expect_identical(f$state$run_count, 0L)
})

test_that("unnamed and duplicate reconciliation keys refuse without allocation", {
  f <- bounded_execute_fixture()
  on.exit(unlink(f$root, recursive = TRUE))
  item <- list(reconciled_attempt = "attempt-1", reconciliation = "review")
  bad <- list(list(item), stats::setNames(list(item, item),
                                        c("producer", "producer")))
  for (value in bad) {
    expect_error(bounded_execute_at(f, reconciliations = value),
                 "Invalid stage reconciliations")
    expect_false(dir.exists(f$graph))
    expect_false(dir.exists(f$control))
  }
  expect_identical(f$state$run_count, 0L)
})

test_that("nested tracked inputs are traversed and overlap refuses before mutation", {
  f <- bounded_execute_fixture()
  on.exit(unlink(f$root, recursive = TRUE))
  dir.create(f$graph)
  input <- file.path(f$graph, "input.txt")
  writeLines("protected input", input)
  before <- rf_file_hash(input)
  plan <- f$plan
  plan$stages$producer$declaration$args <- list(nested = list(
    input = reflow_imaging_input(input)))
  expect_error(bounded_execute_at(f, plan = plan), "Generation overlaps tracked input")
  expect_identical(list.files(f$graph, all.files = TRUE, no.. = TRUE), "input.txt")
  expect_identical(rf_file_hash(input), before)
  expect_false(dir.exists(f$control))
  expect_identical(f$state$run_count, 0L)
  # The inverse relationship is also forbidden: a generation within an input tree.
  plan$stages$producer$declaration$args <- list(reflow_imaging_input(f$root))
  expect_error(bounded_execute_at(f, plan = plan), "Generation overlaps tracked input")
  expect_identical(rf_file_hash(input), before)
})

test_that("disjoint tracked input traversal permits literal scheduler success", {
  f <- bounded_execute_fixture()
  on.exit(unlink(f$root, recursive = TRUE))
  input <- file.path(f$root, "input.txt")
  writeLines("retained input", input)
  before <- rf_file_hash(input)
  plan <- f$plan
  plan$stages$producer$declaration$args <- list(nested = list(
    input = reflow_imaging_input(input)))
  result <- bounded_execute_at(f, plan = plan)
  expect_identical(names(result$nodes), c("producer", "consumer"))
  expect_identical(f$state$run_count, 2L)
  expect_identical(rf_file_hash(input), before)
  bounded_execute_unlocked(f)
})

test_that("overlapping prior generation refuses before reading history", {
  f <- bounded_execute_fixture()
  on.exit(unlink(f$root, recursive = TRUE))
  dir.create(f$graph)
  marker <- file.path(f$graph, "protected")
  writeLines("untouched", marker)
  before <- rf_file_hash(marker)
  expect_error(bounded_execute_at(f, previous = f$graph), "Generations overlap")
  expect_false("history" %in% f$state$events)
  expect_identical(list.files(f$graph, all.files = TRUE, no.. = TRUE), "protected")
  expect_identical(rf_file_hash(marker), before)
  expect_false(dir.exists(f$control))
  expect_identical(f$state$run_count, 0L)
})

test_that("fresh generation refuses existing file and directory without overwrite", {
  f <- bounded_execute_fixture()
  on.exit(unlink(f$root, recursive = TRUE))
  writeLines("existing file", f$graph)
  before <- rf_file_hash(f$graph)
  expect_error(f$run(), "Fresh generation required")
  expect_identical(rf_file_hash(f$graph), before)
  other <- file.path(f$root, "existing-directory")
  dir.create(other)
  writeLines("existing directory", file.path(other, "marker"))
  marker <- rf_file_hash(file.path(other, "marker"))
  expect_error(bounded_execute_at(f, other), "Fresh generation required")
  expect_identical(list.files(other, all.files = TRUE, no.. = TRUE), "marker")
  expect_identical(rf_file_hash(file.path(other, "marker")), marker)
  expect_false(dir.exists(f$control))
  expect_identical(f$state$run_count, 0L)
})

test_that("generation loss during declaration validation refuses without recreating it", {
  f <- bounded_execute_fixture()
  on.exit(unlink(f$root, recursive = TRUE))
  f$run()
  node_before <- readRDS(file.path(f$graph, "nodes.rds"))
  definition_before <- readRDS(file.path(f$graph, "definition.rds"))
  count <- f$state$run_count
  moved <- file.path(f$root, "externally-moved-generation")
  original <- f$env$rfg_definition
  f$env$rfg_definition <- function(plan) {
    value <- original(plan)
    # Controlled external filesystem race after the initial existence/lock check.
    stopifnot(file.rename(f$graph, moved))
    value
  }
  expect_error(f$run(FALSE), "Generation directory missing")
  expect_false(file.exists(f$graph))
  expect_identical(readRDS(file.path(moved, "nodes.rds")), node_before)
  expect_identical(readRDS(file.path(moved, "definition.rds")), definition_before)
  expect_identical(f$state$run_count, count)
  expect_false(dir.exists(file.path(f$control, ".lock")))
  # A lock moved outside its original path is not silently found or removed.
  expect_true(dir.exists(file.path(moved, ".lock")))
})
