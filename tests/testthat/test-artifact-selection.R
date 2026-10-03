test_that("installed writers consume data and unchanged selections do not copy", {
  f <- selector_fixture()
  expect_error(reflow_artifact_select(f$make(), f$registry), "initialization")
  p <- reflow_artifact_select(f$make(), f$registry, initialize = TRUE)
  expect_identical(p$status, "REFERENCE_REQUIRES_VERIFICATION")
  d <- reflow_artifact_selection_verify(p, f$make())
  expect_identical(readLines(file.path(d$nodes$consumer$descriptor$bundle, "value.txt")),
                   c("literal", "second"))
  snapshot <- selector_snapshot(f$registry)
  expect_identical(reflow_artifact_select(f$make(), f$registry), p)
  expect_identical(selector_snapshot(f$registry), snapshot)
  previous <- selector_snapshot(p$generation)
  saveRDS(c("changed", "third"), f$input)
  expect_error(reflow_artifact_selection_verify(p, f$make()), "request changed")
  q <- reflow_artifact_select(f$make(), f$registry)
  next_graph <- reflow_artifact_selection_verify(q, f$make())
  expect_false(identical(p$generation, q$generation))
  expect_identical(next_graph$nodes$independent, d$nodes$independent)
  expect_false(dir.exists(file.path(q$generation, "independent")))
  expect_identical(selector_snapshot(p$generation), previous)
  expect_identical(readLines(file.path(next_graph$nodes$consumer$descriptor$bundle, "value.txt")),
                   c("changed", "third"))
  expect_error(reflow_artifact_selection_verify(p, f$make()), "stale")
})

test_that("absent graph after PENDING cannot be automatically dispatched", {
  f <- selector_fixture()
  fault <- "PENDING"
  local_mocked_bindings(rfs_checkpoint = function(point) {
    if (identical(point, fault)) stop("controlled interruption")
  }, .package = "reflowR")
  expect_error(reflow_artifact_select(f$make(), f$registry, TRUE), "controlled")
  fault <- NULL
  state <- rfs_read(f$registry)
  expect_false(file.exists(state$active$generation))
  before <- selector_snapshot(f$registry)
  expect_error(reflow_artifact_select(f$make(), f$registry), "Unresolved")
  expect_error(reflow_artifact_select(f$make(), f$registry,
    reconciliation = selector_retry(f$registry, "resume_incomplete")), "absent graph")
  expect_identical(selector_snapshot(f$registry), before)
  expect_null(reflow_artifact_select(f$make(), f$registry,
    reconciliation = selector_retry(f$registry, "close_failed")))
  p <- reflow_artifact_select(f$make(), f$registry)
  expect_false(identical(p$generation, state$active$generation))
})

test_that("complete publications recover each valid boundary without another writer", {
  for (boundary in c("GRAPH_COMPLETE", "COMPLETED", "INTENT",
                     "POINTER_STAGED", "POINTER_RENAMED")) {
    f <- selector_fixture()
    fault <- boundary
    local_mocked_bindings(rfs_checkpoint = function(point) {
      if (identical(point, fault)) stop("controlled interruption")
    }, .package = "reflowR")
    expect_error(reflow_artifact_select(f$make(), f$registry, TRUE), "controlled")
    fault <- NULL
    s <- rfs_read(f$registry)
    artifacts <- selector_snapshot(s$active$generation)
    before <- selector_snapshot(f$registry)
    expect_error(reflow_artifact_select(f$make(), f$registry), "Unresolved")
    expect_identical(selector_snapshot(f$registry), before)
    retry <- selector_retry(f$registry)
    wrong <- retry
    wrong$pending_hash <- strrep("0", 64)
    expect_error(reflow_artifact_select(f$make(), f$registry, reconciliation = wrong), "binding")
    expect_identical(selector_snapshot(f$registry), before)
    if (boundary == "POINTER_RENAMED") {
      expect_error(reflow_artifact_select(f$make(), f$registry,
        reconciliation = selector_retry(f$registry, "close_failed")), "only publish_complete")
      expect_identical(selector_snapshot(f$registry), before)
    }
    p <- reflow_artifact_select(f$make(), f$registry, reconciliation = retry)
    expect_identical(selector_snapshot(p$generation), artifacts)
    expect_identical(rfs_read(f$registry)$phase, "IDLE")
    expect_identical(reflow_artifact_select(f$make(), f$registry), p)
  }
})

test_that("event staging residue requires external inspection, never guessed recovery", {
  f <- selector_fixture()
  local_mocked_bindings(rfs_checkpoint = function(point) {
    if (identical(point, "PENDING_STAGED")) stop("controlled interruption")
  }, .package = "reflowR")
  expect_error(reflow_artifact_select(f$make(), f$registry, TRUE), "controlled")
  before <- selector_snapshot(f$registry)
  expect_error(reflow_artifact_select(f$make(), f$registry), "staging residue")
  expect_identical(selector_snapshot(f$registry), before)
  expect_length(list.files(file.path(f$registry, "generations")), 0L)
})

test_that("current journal and payload corruption refuse with exact preservation", {
  for (kind in c("current", "event", "header", "payload", "journal", "foreign", "orphan")) {
    f <- selector_fixture()
    p <- reflow_artifact_select(f$make(), f$registry, TRUE)
    if (kind == "current") unlink(file.path(f$registry, "current.rds"))
    if (kind == "event") unlink(rfs_event_path(f$registry, 2L))
    if (kind == "header") saveRDS(list(wrong = TRUE), file.path(f$registry, "registry.rds"))
    if (kind == "journal") unlink(file.path(p$generation, "nodes.rds"))
    if (kind == "payload") {
      d <- reflow_artifact_selection_verify(p, f$make())
      writeLines("tampered", file.path(d$nodes$consumer$descriptor$bundle, "value.txt"))
    }
    if (kind == "foreign") dir.create(file.path(f$registry, "unexpected-empty"))
    if (kind == "orphan") dir.create(file.path(f$registry, "generations", "orphan"))
    before <- selector_snapshot(f$registry)
    expect_error(reflow_artifact_select(f$make(), f$registry))
    expect_identical(selector_snapshot(f$registry), before)
  }
})

test_that("input drift before publication preserves pending generation and old pointer", {
  f <- selector_fixture()
  fault <- TRUE
  local_mocked_bindings(rfs_checkpoint = function(point) {
    if (identical(point, "POINTER_STAGED") && fault) {
      fault <<- FALSE
      saveRDS("drift", f$input)
    }
  }, .package = "reflowR")
  expect_error(reflow_artifact_select(f$make(), f$registry, TRUE), "Request changed")
  expect_null(rfs_current(f$registry))
  before <- selector_snapshot(f$registry)
  expect_error(reflow_artifact_select(f$make(), f$registry,
    reconciliation = selector_retry(f$registry)), "Pending request changed")
  expect_identical(selector_snapshot(f$registry), before)
})

test_that("same registry real contenders cannot both dispatch", {
  f <- selector_fixture()
  skip_on_cran()
  gate <- file.path(f$root, "release")
  reached <- file.path(f$root, "reached")
  local_mocked_bindings(rfs_checkpoint = function(point) {
    if (identical(point, "PENDING")) {
      file.create(reached)
      deadline <- Sys.time() + 10
      while (!file.exists(gate) && Sys.time() < deadline) Sys.sleep(0.02)
      if (!file.exists(gate)) stop("Owned contender timed out")
    }
  }, .package = "reflowR")
  job <- parallel::mcparallel(reflow_artifact_select(f$make(), f$registry, TRUE))
  collected <- FALSE
  on.exit({
    if (!collected) {
      file.create(gate)
      parallel::mccollect(job, wait = TRUE)
    }
  }, add = TRUE)
  deadline <- Sys.time() + 5
  while (!file.exists(reached) && Sys.time() < deadline) Sys.sleep(0.02)
  expect_true(file.exists(reached))
  before <- selector_snapshot(f$registry)
  expect_error(reflow_artifact_select(f$make(), f$registry), "Registry locked")
  expect_identical(selector_snapshot(f$registry), before)
  file.create(gate)
  result <- parallel::mccollect(job, wait = TRUE)
  collected <- TRUE
  expect_length(result, 1L)
  expect_false(inherits(result[[1L]], "try-error"))
  expect_identical(result[[1L]]$status, "REFERENCE_REQUIRES_VERIFICATION")
})

test_that("a completed successor and stale prior pointer require publication recovery", {
  f <- selector_fixture()
  first <- reflow_artifact_select(f$make(), f$registry, TRUE)
  prior <- selector_snapshot(first$generation)
  saveRDS("new value", f$input)
  fault <- TRUE
  local_mocked_bindings(rfs_checkpoint = function(point) {
    if (identical(point, "COMPLETED") && fault) stop("controlled interruption")
  }, .package = "reflowR")
  expect_error(reflow_artifact_select(f$make(), f$registry), "controlled")
  fault <- FALSE
  expect_identical(rfs_current(f$registry), first)
  state <- rfs_read(f$registry)
  successor <- selector_snapshot(state$active$generation)
  expect_error(reflow_artifact_selection_verify(first, f$make()), "Incomplete")
  second <- reflow_artifact_select(f$make(), f$registry,
    reconciliation = selector_retry(f$registry))
  expect_false(identical(second$generation, first$generation))
  expect_identical(selector_snapshot(first$generation), prior)
  expect_identical(selector_snapshot(second$generation), successor)
})

test_that("missing initialization and special metadata never dispatch a writer", {
  f <- selector_fixture()
  dir.create(f$registry)
  before <- selector_snapshot(f$registry)
  expect_error(reflow_artifact_select(f$make(), f$registry))
  expect_identical(selector_snapshot(f$registry), before)
  g <- selector_fixture()
  p <- reflow_artifact_select(g$make(), g$registry, TRUE)
  current <- file.path(g$registry, "current.rds")
  unlink(current)
  expect_true(file.symlink(file.path(g$root, "absent"), current))
  expect_error(reflow_artifact_select(g$make(), g$registry), "links")
  expect_true(nzchar(Sys.readlink(current)))
  unlink(current)
  skip_if(!nzchar(Sys.which("mkfifo")))
  status <- system2(Sys.which("mkfifo"), shQuote(current))
  expect_identical(status, 0L)
  expect_error(reflow_artifact_select(g$make(), g$registry), "regular")
  expect_identical(as.character(fs::file_info(current, follow = FALSE)$type), "FIFO")
})

test_that("completed then failed siblings retain immutable integrity obligations", {
  for (damage in c("payload", "READY")) {
    f <- selector_fixture()
    original_plan <- f$make()
    published <- reflow_artifact_select(original_plan, f$registry, TRUE)
    prior <- selector_snapshot(published$generation)
    saveRDS("unpublished changed data", f$input)
    fault <- TRUE
    local_mocked_bindings(rfs_checkpoint = function(point) {
      if (identical(point, "COMPLETED") && fault) stop("controlled interruption")
    }, .package = "reflowR")
    expect_error(reflow_artifact_select(f$make(), f$registry), "controlled")
    fault <- FALSE
    state <- rfs_read(f$registry)
    abandoned <- state$active$generation
    ready_hash <- state$complete$ready
    expect_null(reflow_artifact_select(f$make(), f$registry,
      reconciliation = selector_retry(f$registry, "close_failed")))
    expect_identical(rfs_current(f$registry), published)
    bindings <- rfs_read(f$registry)$completed_bindings
    expect_length(bindings, 2L)
    expect_identical(bindings[[2L]]$directory, abandoned)
    expect_identical(bindings[[2L]]$ready_hash, ready_hash)
    expect_identical(selector_snapshot(published$generation), prior)
    if (damage == "READY") {
      saveRDS(list(corrupt = TRUE), file.path(abandoned, "READY.rds"))
    } else {
      journal <- readRDS(file.path(abandoned, "nodes.rds"))
      bundle <- journal$consumer$descriptor$bundle
      writeLines("corrupt unpublished payload", file.path(bundle, "value.txt"))
    }
    before <- selector_snapshot(f$registry)
    entries <- list.files(file.path(f$registry, "generations"))
    expected_error <- if (damage == "READY") "Prior READY changed" else "Corrupt accepted bundle"
    expect_error(reflow_artifact_select(f$make(), f$registry), expected_error)
    expect_identical(selector_snapshot(f$registry), before)
    expect_identical(list.files(file.path(f$registry, "generations")), entries)
    expect_error(reflow_artifact_selection_verify(published, f$make()), expected_error)
    expect_identical(selector_snapshot(f$registry), before)
    expect_identical(selector_snapshot(published$generation), prior)
    expect_false(dir.exists(paste0(f$registry, ".lock")))
    expect_false(any(dir.exists(file.path(f$registry, "generations", entries, ".lock"))))
  }
})

test_that("selector entry refusals preserve existing data and never initialize", {
  f <- selector_fixture()
  before <- selector_snapshot(f$root)
  expect_error(reflow_artifact_select(f$make(), file.path(f$root, "missing", "registry")),
               "parent missing")
  expect_error(reflow_artifact_select(f$make(), paste0(f$root, "/./registry")), "Canonical")
  expect_error(reflow_artifact_select(f$make(), f$registry, initialize = NA), "Plain initialize")
  expect_error(reflow_artifact_select(f$make(), f$input, initialize = TRUE), "overlaps")
  expect_error(reflow_artifact_select(f$make(), f$registry, TRUE,
    reconciliation = list()), "absent registry")
  expect_identical(selector_snapshot(f$root), before)
  expect_false(file.exists(f$registry))
  expect_false(file.exists(paste0(f$registry, ".lock")))
})

test_that("journal semantic corruption refuses without changing any surviving bytes", {
  f <- selector_fixture()
  p <- reflow_artifact_select(f$make(), f$registry, TRUE)
  payload_before <- selector_snapshot(p$generation)
  dispatched <- character()
  writer_calls <- 0L
  actual_call <- rf_call
  local_mocked_bindings(rf_call = function(stage, args) {
    writer_calls <<- writer_calls + 1L
    actual_call(stage, args)
  }, .package = "reflowR")
  real_writer <- reflow_artifact_run
  local_mocked_bindings(reflow_artifact_run = function(spec, directory, ...) {
    dispatched <<- c(dispatched, directory)
    real_writer(spec, directory, ...)
  }, .package = "reflowR")
  events <- lapply(1:4, function(i) readRDS(rfs_event_path(f$registry, i)))
  header_path <- file.path(f$registry, "registry.rds")
  header <- readRDS(header_path)
  current <- readRDS(file.path(f$registry, "current.rds"))
  cases <- list(
    header = "Registry header differs",
    chain = "Event chain differs",
    pending = "Invalid PENDING",
    definition = "Invalid pending definition",
    binding = "Event pending binding differs",
    completed = "Invalid COMPLETED",
    intent = "Invalid INTENT",
    published = "Invalid PUBLISHED",
    failed = "Invalid FAILED",
    unknown = "Unknown selector",
    hash = "Invalid selector hash",
    pointer = "Invalid selector pointer",
    staging = "Pointer staging differs"
  )
  for (kind in names(cases)) {
    saveRDS(header, header_path, version = 2)
    saveRDS(current, file.path(f$registry, "current.rds"), version = 2)
    unlink(file.path(f$registry, "pointer.tmp.rds"))
    e <- events
    if (kind == "header") {
      x <- header
      x$schema <- "foreign"
      saveRDS(x, header_path, version = 2)
    }
    if (kind == "chain") e[[1]]$sequence <- 2L
    if (kind == "pending") e[[1]]$ready <- strrep("0", 64)
    if (kind == "definition") e[[1]]$definition <- list(schema = "bad")
    if (kind == "binding") e[[2]]$generation <- "foreign"
    if (kind == "completed") e[[3]]$type <- "COMPLETED"
    if (kind == "intent") e[[2]]$type <- "INTENT"
    if (kind == "published") e[[2]]$type <- "PUBLISHED"
    if (kind == "failed") e[[2]]$type <- "FAILED"
    if (kind == "unknown") e[[2]]$type <- "UNKNOWN"
    if (kind == "hash") e[[2]]$ready <- "not-a-hash"
    # Rechain only to reach the semantic transition guard, not to bypass it.
    previous <- NULL
    for (i in seq_along(e)) {
      e[[i]]["previous"] <- list(previous)
      saveRDS(e[[i]], rfs_event_path(f$registry, i), version = 2)
      previous <- rf_file_hash(rfs_event_path(f$registry, i))
    }
    if (kind == "pointer") {
      x <- current
      x$event <- 0L
      saveRDS(x, file.path(f$registry, "current.rds"), version = 2)
    }
    if (kind == "staging") saveRDS(current, file.path(f$registry, "pointer.tmp.rds"))
    before <- selector_snapshot(f$registry)
    expect_error(reflow_artifact_select(f$make(), f$registry), cases[[kind]])
    expect_length(dispatched, 0L)
    expect_identical(writer_calls, 0L)
    expect_identical(selector_snapshot(f$registry), before)
    expect_false(file.exists(paste0(f$registry, ".lock")))
  }
  expect_identical(selector_snapshot(p$generation), payload_before)
})

test_that("missing directories and occupied generation files are retained on refusal", {
  for (kind in c("events", "generations", "generation-file")) {
    f <- selector_fixture()
    fault <- TRUE
    local_mocked_bindings(rfs_checkpoint = function(point) {
      if (fault && identical(point, "PENDING")) stop("owned interruption")
    }, .package = "reflowR")
    expect_error(reflow_artifact_select(f$make(), f$registry, TRUE), "owned interruption")
    fault <- FALSE
    state <- rfs_read(f$registry)
    if (kind == "generation-file") {
      writeLines("foreign generation", state$active$generation)
    } else {
      unlink(file.path(f$registry, kind), recursive = TRUE)
    }
    before <- selector_snapshot(f$registry)
    expect_error(reflow_artifact_select(f$make(), f$registry),
      if (kind == "generation-file") "Generation must be a directory" else "directory missing")
    expect_identical(selector_snapshot(f$registry), before)
  }
})

test_that("reconciliation action guards cannot dispatch or replace a pointer", {
  f <- selector_fixture()
  fault <- TRUE
  local_mocked_bindings(rfs_checkpoint = function(point) {
    if (fault && identical(point, "PENDING")) stop("owned interruption")
  }, .package = "reflowR")
  expect_error(reflow_artifact_select(f$make(), f$registry, TRUE), "owned interruption")
  fault <- FALSE
  before <- selector_snapshot(f$registry)
  retry <- selector_retry(f$registry)
  retry$action <- "guess"
  expect_error(reflow_artifact_select(f$make(), f$registry, reconciliation = retry), "action")
  retry <- selector_retry(f$registry)
  retry$reconciliations <- list(unrequested = TRUE)
  expect_error(reflow_artifact_select(f$make(), f$registry, reconciliation = retry), "action")
  expect_identical(selector_snapshot(f$registry), before)
  reflow_artifact_select(f$make(), f$registry,
    reconciliation = selector_retry(f$registry, "close_failed"))
  closed <- selector_snapshot(f$registry)
  expect_error(reflow_artifact_select(f$make(), f$registry, reconciliation = retry), "No pending")
  expect_identical(selector_snapshot(f$registry), closed)
})

test_that("real event and pointer destination races preserve forensic residue", {
  actual_call <- rf_call
  real_writer <- reflow_artifact_run
  for (kind in c("event-rename", "pointer-rename", "staging-tamper", "pointer-drift",
                 "post-publication-input")) {
    f <- selector_fixture()
    fired <- 0L
    graph_before <- NULL
    generation <- NULL
    dispatches <- character()
    writer_calls <- 0L
    local_mocked_bindings(rf_call = function(stage, args) {
      writer_calls <<- writer_calls + 1L
      actual_call(stage, args)
    }, .package = "reflowR")
    local_mocked_bindings(reflow_artifact_run = function(spec, directory, ...) {
      dispatches <<- c(dispatches, basename(directory))
      real_writer(spec, directory, ...)
    }, .package = "reflowR")
    local_mocked_bindings(rfs_checkpoint = function(point) {
      wanted <- if (kind == "event-rename") "PENDING_STAGED" else
        if (kind == "post-publication-input") "POINTER_RENAMED" else "POINTER_STAGED"
      if (identical(point, wanted) && fired == 0L) {
        fired <<- fired + 1L
        if (kind != "event-rename") {
          generation <<- rfs_read(f$registry)$active$generation
          graph_before <<- selector_snapshot(generation)
        }
        if (kind == "event-rename") dir.create(rfs_event_path(f$registry, 1L))
        if (kind == "pointer-rename") dir.create(file.path(f$registry, "current.rds"))
        if (kind == "staging-tamper") {
          saveRDS(list(foreign = TRUE), file.path(f$registry, "pointer.tmp.rds"))
        }
        if (kind == "pointer-drift") {
          x <- readRDS(file.path(f$registry, "pointer.tmp.rds"))
          saveRDS(x, file.path(f$registry, "current.rds"))
        }
        if (kind == "post-publication-input") saveRDS("changed after rename", f$input)
      }
    }, .package = "reflowR")
    expected <- switch(kind, "event-rename" = "Event rename failed",
      "pointer-rename" = "regular", "staging-tamper" = "Pointer staging differs",
      "pointer-drift" = "Pointer changed before rename",
      "post-publication-input" = "Request changed after publication")
    if (kind == "event-rename") {
      expect_warning(expect_error(reflow_artifact_select(f$make(), f$registry, TRUE),
                                 expected), "rename")
    } else {
      expect_error(reflow_artifact_select(f$make(), f$registry, TRUE), expected)
    }
    expect_identical(fired, 1L)
    if (!is.null(generation)) expect_identical(selector_snapshot(generation), graph_before)
    journal <- list.files(file.path(f$registry, "events"), full.names = TRUE)
    journal <- journal[!dir.exists(journal)]
    types <- vapply(journal, function(x) readRDS(x)$type, character(1))
    expect_false("PUBLISHED" %in% types)
    expect_false(file.exists(paste0(f$registry, ".lock")))
    before <- selector_snapshot(f$registry)
    calls_before <- dispatches
    writers_before <- writer_calls
    expect_error(reflow_artifact_select(f$make(), f$registry))
    expect_identical(dispatches, calls_before)
    expect_identical(writer_calls, writers_before)
    expect_identical(selector_snapshot(f$registry), before)
    expect_true(file.exists(file.path(f$registry,
      if (kind == "event-rename") "event.tmp.rds" else "registry.rds")))
  }
})

test_that("platform and optional capability refusals cannot reach a writer", {
  f <- selector_fixture()
  execute <- rfs_execute
  expect_identical(body(execute), body(rfs_execute))
  expect_identical(formals(execute), formals(rfs_execute))
  env <- new.env(parent = environment(execute))
  environment(execute) <- env
  env$Sys.info <- function() c(sysname = "unsupported")
  before <- selector_snapshot(f$root)
  expect_error(execute(f$make(), f$registry, TRUE, NULL, NULL), "requires Linux")
  expect_identical(selector_snapshot(f$root), before)
  target <- reflow_artifact_target
  expect_identical(body(target), body(reflow_artifact_target))
  expect_identical(formals(target), formals(reflow_artifact_target))
  env <- new.env(parent = environment(target))
  environment(target) <- env
  requested <- character()
  env$requireNamespace <- function(package, quietly) {
    requested <<- c(requested, package)
    FALSE
  }
  expect_error(target("selected", quote(make_plan()), f$registry), "capability unavailable")
  expect_identical(requested, "targets")
  expect_identical(selector_snapshot(f$root), before)
})

test_that("historical definition mismatches release locks and retain graph data", {
  f <- selector_fixture()
  p <- reflow_artifact_select(f$make(), f$registry, TRUE)
  before <- selector_snapshot(f$registry)
  different <- rfg_definition(reflow_artifact_plan(
    reflow_artifact_stage("literal", "base", "writeLines", list(text = "other"),
      outputs = list(con = list(path = "value.txt", type = "file")),
      inventory = c(value = "value.txt"))))
  expect_error(rfs_history(p$generation, p$ready_hash, different), "Historical request")
  expect_identical(selector_snapshot(f$registry), before)
  expect_false(file.exists(paste0(p$generation, ".lock")))
})

test_that("an occupied event staging destination cannot overwrite foreign evidence", {
  f <- selector_fixture()
  fault <- TRUE
  local_mocked_bindings(rfs_checkpoint = function(point) {
    if (fault && identical(point, "PENDING")) stop("owned interruption")
  }, .package = "reflowR")
  expect_error(reflow_artifact_select(f$make(), f$registry, TRUE), "owned interruption")
  fault <- FALSE
  state <- rfs_read(f$registry)
  writeLines("foreign staging evidence", file.path(f$registry, "event.tmp.rds"))
  before <- selector_snapshot(f$registry)
  expect_error(rfs_append(f$registry, state, "FAILED"), "destination occupied")
  expect_identical(selector_snapshot(f$registry), before)
  expect_false(file.exists(state$active$generation))
})

test_that("an interrupted dispatch resumes only after explicit graph reconciliation", {
  f <- selector_fixture()
  original <- reflow_artifact_run
  interrupted <- TRUE
  dispatches <- character()
  completed <- character()
  successful_writer_calls <- 0L
  actual_call <- rf_call
  local_mocked_bindings(rf_call = function(stage, args) {
    value <- actual_call(stage, args)
    successful_writer_calls <<- successful_writer_calls + 1L
    value
  }, .package = "reflowR")
  local_mocked_bindings(reflow_artifact_run = function(spec, directory, ...) {
    dispatches <<- c(dispatches, basename(directory))
    if (interrupted && identical(basename(directory), "consumer")) {
      interrupted <<- FALSE
      # Producer has completed; consumer transport stops before launching its writer.
      stop("owned pre-writer transport interruption")
    }
    result <- original(spec, directory, ...)
    completed <<- c(completed, basename(directory))
    result
  }, .package = "reflowR")
  expect_error(reflow_artifact_select(f$make(), f$registry, TRUE), "owned pre-writer")
  state <- rfs_read(f$registry)
  expect_true(dir.exists(state$active$generation))
  expect_false(file.exists(file.path(state$active$generation, "READY.rds")))
  before <- selector_snapshot(f$registry)
  expect_error(reflow_artifact_select(f$make(), f$registry), "Unresolved")
  expect_identical(selector_snapshot(f$registry), before)
  expect_identical(dispatches, c("independent", "producer", "consumer"))
  expect_identical(completed, c("independent", "producer"))
  expect_identical(successful_writer_calls, 2L)
  journal <- readRDS(file.path(state$active$generation, "nodes.rds"))
  producer <- journal$producer
  independent <- journal$independent
  independent_before <- selector_snapshot(independent$descriptor$directory)
  independent_attempts <- list.files(independent$descriptor$directory, pattern = "^attempt-")
  expect_length(independent_attempts, 1L)
  producer_before <- selector_snapshot(producer$descriptor$directory)
  producer_attempts <- list.files(producer$descriptor$directory, pattern = "^attempt-")
  expect_length(producer_attempts, 1L)
  # No consumer writer child/attempt was launched by this transport failure.
  retry <- selector_retry(f$registry, "resume_incomplete")
  p <- reflow_artifact_select(f$make(), f$registry, reconciliation = retry)
  graph <- reflow_artifact_selection_verify(p, f$make())
  expect_identical(readLines(file.path(graph$nodes$consumer$descriptor$bundle, "value.txt")),
                   c("literal", "second"))
  expect_identical(p$generation, state$active$generation)
  expect_identical(dispatches, c("independent", "producer", "consumer", "consumer"))
  expect_identical(completed, c("independent", "producer", "consumer"))
  expect_identical(successful_writer_calls, 3L)
  expect_identical(graph$nodes$producer, producer)
  expect_identical(graph$nodes$independent, independent)
  expect_identical(selector_snapshot(independent$descriptor$directory), independent_before)
  expect_identical(list.files(independent$descriptor$directory, pattern = "^attempt-"),
                   independent_attempts)
  expect_identical(selector_snapshot(producer$descriptor$directory), producer_before)
  expect_identical(list.files(producer$descriptor$directory, pattern = "^attempt-"),
                   producer_attempts)
  expect_identical(rfs_read(f$registry)$phase, "IDLE")
})

test_that("completion boundary races preserve their exact injected evidence", {
  for (kind in c("request", "definition", "ready", "current")) local({
    mode <- kind
    f <- selector_fixture()
    fired <- 0L
    writer_calls <- 0L
    actual_call <- rf_call
    actual_history <- rfs_history
    actual_read <- rfs_read
    expected_registry <- NULL
    local_mocked_bindings(rf_call = function(stage, args) {
      value <- actual_call(stage, args)
      writer_calls <<- writer_calls + 1L
      value
    }, .package = "reflowR")
    local_mocked_bindings(rfs_checkpoint = function(point) {
      if (mode == "request" && point == "GRAPH_COMPLETE" && fired == 0L) {
        fired <<- 1L
        saveRDS("changed between completion and publication", f$input)
        expected_registry <<- selector_snapshot(f$registry)
      }
      if (mode == "ready" && point == "COMPLETED" && fired == 0L) {
        fired <<- 1L
        path <- rfs_event_path(f$registry, 2L)
        record <- readRDS(path)
        record$ready <- strrep("0", 64)
        saveRDS(record, path, version = 2)
        expected_registry <<- selector_snapshot(f$registry)
      }
    }, .package = "reflowR")
    local_mocked_bindings(rfs_history = function(directory, ready = NULL, definition = NULL) {
      value <- actual_history(directory, ready, definition)
      if (mode == "definition" && fired == 0L) {
        fired <<- 1L
        path <- file.path(directory, "definition.rds")
        record <- readRDS(path)
        record$schema <- "injected foreign schema"
        saveRDS(record, path, version = 2)
        expected_registry <<- selector_snapshot(f$registry)
      }
      value
    }, .package = "reflowR")
    local_mocked_bindings(rfs_read = function(registry) {
      value <- actual_read(registry)
      if (mode == "current" && identical(value$phase, "INTENT") && fired == 0L) {
        fired <<- 1L
        pointer <- value$pointer
        pointer$request_hash <- strrep("0", 64)
        saveRDS(pointer, file.path(registry, "current.rds"), version = 2)
        expected_registry <<- selector_snapshot(registry)
      }
      value
    }, .package = "reflowR")
    expected <- switch(mode, request = "Pending request changed",
      definition = "Completed graph definition differs", ready = "Completed READY changed",
      current = "Pointer changed before publication")
    expect_error(reflow_artifact_select(f$make(), f$registry, TRUE), expected)
    expect_identical(fired, 1L)
    expect_identical(writer_calls, 3L)
    expect_identical(selector_snapshot(f$registry), expected_registry)
    expect_false(dir.exists(paste0(f$registry, ".lock")))
    events <- list.files(file.path(f$registry, "events"), full.names = TRUE)
    expect_false("PUBLISHED" %in% vapply(events, function(p) readRDS(p)$type, character(1)))
  })
})

test_that("actual pointer rename failure retains stage and completed payloads", {
  f <- selector_fixture()
  complete <- rfs_complete
  expect_identical(body(complete), body(rfs_complete))
  expect_identical(formals(complete), formals(rfs_complete))
  env <- new.env(parent = environment(complete))
  environment(complete) <- env
  actual_rename <- base::file.rename
  fired <- 0L
  expected_registry <- NULL
  writer_calls <- 0L
  actual_call <- rf_call
  local_mocked_bindings(rf_call = function(stage, args) {
    value <- actual_call(stage, args)
    writer_calls <<- writer_calls + 1L
    value
  }, .package = "reflowR")
  env$file.rename <- function(from, to) {
    if (identical(to, file.path(f$registry, "current.rds"))) {
      fired <<- fired + 1L
      expect_true(dir.create(to))
      expected_registry <<- selector_snapshot(f$registry)
    }
    actual_rename(from, to)
  }
  local_mocked_bindings(rfs_complete = complete, .package = "reflowR")
  expect_warning(expect_error(reflow_artifact_select(f$make(), f$registry, TRUE),
                              "Pointer rename failed"), "rename")
  expect_identical(fired, 1L)
  expect_identical(writer_calls, 3L)
  expect_identical(selector_snapshot(f$registry), expected_registry)
  expect_true(dir.exists(file.path(f$registry, "current.rds")))
  expect_true(file.exists(file.path(f$registry, "pointer.tmp.rds")))
  expect_false(file.exists(rfs_event_path(f$registry, 4L)))
  expect_false(dir.exists(paste0(f$registry, ".lock")))
})

test_that("equal staging residue after pointer rename still forbids publication", {
  f <- selector_fixture()
  interrupt <- TRUE
  writer_calls <- 0L
  actual_call <- rf_call
  local_mocked_bindings(rf_call = function(stage, args) {
    value <- actual_call(stage, args)
    writer_calls <<- writer_calls + 1L
    value
  }, .package = "reflowR")
  local_mocked_bindings(rfs_checkpoint = function(point) {
    if (interrupt && identical(point, "POINTER_RENAMED")) {
      interrupt <<- FALSE
      pointer <- readRDS(file.path(f$registry, "current.rds"))
      saveRDS(pointer, file.path(f$registry, "pointer.tmp.rds"), version = 2)
      stop("owned interruption after real pointer rename")
    }
  }, .package = "reflowR")
  expect_error(reflow_artifact_select(f$make(), f$registry, TRUE), "owned interruption")
  expect_identical(writer_calls, 3L)
  pointer <- rfs_current(f$registry)
  expect_identical(pointer$status, "REFERENCE_REQUIRES_VERIFICATION")
  before <- selector_snapshot(f$registry)
  expect_error(reflow_artifact_select(f$make(), f$registry,
    reconciliation = selector_retry(f$registry)), "Unexpected staging after pointer replacement")
  expect_identical(writer_calls, 3L)
  expect_identical(rfs_current(f$registry), pointer)
  expect_identical(selector_snapshot(f$registry), before)
  expect_false(file.exists(rfs_event_path(f$registry, 4L)))
  expect_false(dir.exists(paste0(f$registry, ".lock")))
})

test_that("partial registry initialization retains the exact filesystem conflict", {
  f <- selector_fixture()
  execute <- rfs_execute
  expect_identical(body(execute), body(rfs_execute))
  expect_identical(formals(execute), formals(rfs_execute))
  env <- new.env(parent = environment(execute))
  environment(execute) <- env
  actual_create <- base::dir.create
  fired <- 0L
  writer_calls <- 0L
  marker_hash <- NULL
  actual_call <- rf_call
  local_mocked_bindings(rf_call = function(stage, args) {
    writer_calls <<- writer_calls + 1L
    actual_call(stage, args)
  }, .package = "reflowR")
  conflict <- file.path(f$registry, "generations")
  env$dir.create <- function(path, ...) {
    if (identical(path, conflict)) {
      fired <<- fired + 1L
      writeLines("independently created conflicting file", path)
      marker_hash <<- rf_file_hash(path)
    }
    actual_create(path, ...)
  }
  expect_warning(expect_error(execute(f$make(), f$registry, TRUE, NULL, NULL),
                              "Registry initialization failed"), "already exists")
  expect_identical(fired, 1L)
  expect_identical(writer_calls, 0L)
  expect_identical(sort(list.files(f$registry)), c("events", "generations"))
  expect_true(dir.exists(file.path(f$registry, "events")))
  expect_identical(rf_file_hash(conflict), marker_hash)
  expect_false(file.exists(file.path(f$registry, "registry.rds")))
  expect_false(dir.exists(paste0(f$registry, ".lock")))
})
