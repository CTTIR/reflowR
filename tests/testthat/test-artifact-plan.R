graph_stage <- function(id, args = list()) {
  reflow_artifact_stage(id, "graphfixture", "write_value", args,
    outputs = list(path = list(
      path = "value.txt",
      type = "file"
    )),
    inventory = c(value = "value.txt")
  )
}

test_that("deferred references enforce a canonical dependency graph", {
  a <- graph_stage("a")
  b <- graph_stage("b", list(input = reflow_artifact_ref("a", "value")))
  expect_identical(reflow_artifact_plan(a, b), reflow_artifact_plan(b, a))
  expect_error(reflow_artifact_plan(a, a), "Duplicate")
  expect_error(reflow_artifact_plan(b), "Unknown")
  expect_error(reflow_artifact_plan(a, graph_stage("b", list(
    input = reflow_artifact_ref("a", "absent")
  ))), "Unknown")
  expect_error(reflow_artifact_plan(graph_stage("a", list(
    input = reflow_artifact_ref("a", "value")
  ))), "Cyclic")
  expect_error(reflow_artifact_ref("a", "value", NA), "Invalid")
  expect_error(reflow_artifact_ref("a", matrix("value")), "plain")
  expect_error(graph_stage("bad-name"), "Invalid")
  expect_error(graph_stage("a", list(x = reflow_imaging_ref("b"))), "Object")
})

test_that("installed writers consume files and immutable generations reuse safely", {
  root <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
  root <- tempfile("artifact graph ", root)
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  lib <- file.path(root, "library")
  dir.create(lib)
  log <- file.path(root, "install.log")
  code <- system2(file.path(R.home("bin"), "R"),
    c(
      "CMD", "INSTALL", "--no-byte-compile",
      paste0("--library=", shQuote(lib)),
      shQuote(test_path("fixtures", "artifact-graph-writer"))
    ),
    stdout = log, stderr = log
  )
  expect_identical(code, 0L)
  old_lib <- .libPaths()
  .libPaths(c(lib, old_lib))
  on.exit(.libPaths(old_lib), add = TRUE)
  on.exit(unloadNamespace("graphfixture"), add = TRUE)
  text <- function(node) readLines(file.path(node$descriptor$bundle, "value.txt"))
  make <- function(value = "beta") {
    reflow_artifact_plan(
      graph_stage("b", list(
        input = reflow_artifact_ref("a", "value"),
        value = value
      )),
      graph_stage("independent", list(value = "untouched")), graph_stage("a")
    )
  }
  p <- make()
  d1 <- file.path(root, "one")
  first <- reflow_artifact_plan_run(p, d1)
  expect_identical(text(first$nodes$b), "alpha beta")
  expect_identical(text(first$nodes$independent), "untouched")
  snapshot <- function(path) {
    files <- list.files(path,
      recursive = TRUE, full.names = TRUE,
      all.files = TRUE, no.. = TRUE
    )
    stats::setNames(vapply(files, rf_file_hash, character(1)), files)
  }
  initial <- snapshot(d1)
  expect_identical(reflow_artifact_plan_resume(p, d1), first)
  expect_identical(snapshot(d1), initial)
  second <- reflow_artifact_plan_run(make("changed"), file.path(root, "two"), d1)
  expect_identical(text(second$nodes$b), "alpha changed")
  expect_identical(second$nodes$a, first$nodes$a)
  expect_identical(second$nodes$independent, first$nodes$independent)
  expect_false(identical(second$nodes$b$descriptor, first$nodes$b$descriptor))
  expect_identical(snapshot(d1), initial)
  expect_error(reflow_artifact_plan_resume(make("changed"), d1), "changed")
  expect_identical(snapshot(d1), initial)

  # A rejected resume leaves all remaining evidence and accepted bytes intact.
  for (name in c("nodes.rds", "previous.rds")) {
    path <- file.path(d1, name)
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    unlink(path)
    before <- snapshot(d1)
    expect_error(reflow_artifact_plan_resume(p, d1))
    expect_identical(snapshot(d1), before)
    writeBin(bytes, path)
  }
  foreign <- file.path(d1, "foreign.txt")
  writeLines("foreign", foreign)
  before <- snapshot(d1)
  expect_error(reflow_artifact_plan_resume(p, d1), "inventory")
  expect_identical(snapshot(d1), before)
  unlink(foreign)
  value <- file.path(first$nodes$a$descriptor$bundle, "value.txt")
  writeLines("corrupt", value)
  before <- snapshot(d1)
  expect_error(reflow_artifact_plan_resume(p, d1), "Corrupt")
  expect_identical(snapshot(d1), before)
  writeLines("alpha", value)

  # A completed new generation still authenticates its pinned predecessor.
  ready_path <- file.path(d1, "READY.rds")
  ready_bytes <- readBin(ready_path, "raw", n = file.info(ready_path)$size)
  writeLines("tampered", ready_path)
  before <- snapshot(file.path(root, "two"))
  expect_error(reflow_artifact_plan_resume(
    make("changed"),
    file.path(root, "two")
  ), "Prior READY|unknown input format")
  expect_identical(snapshot(file.path(root, "two")), before)
  writeBin(ready_bytes, ready_path)
  expect_identical(reflow_artifact_plan_resume(
    make("changed"),
    file.path(root, "two")
  ), second)
  previous_path <- file.path(d1, "previous.rds")
  saveRDS(list(directory = "foreign", ready_hash = paste(rep("0", 64),
    collapse = ""
  )), previous_path)
  before <- snapshot(d1)
  expect_error(reflow_artifact_plan_resume(p, d1), "changed")
  expect_identical(snapshot(d1), before)
  saveRDS(NULL, previous_path)

  shadow <- file.path(root, "two", "a")
  dir.create(shadow)
  writeLines("foreign", file.path(shadow, "foreign.txt"))
  before <- snapshot(file.path(root, "two"))
  expect_error(reflow_artifact_plan_resume(
    make("changed"),
    file.path(root, "two")
  ), "shadow")
  expect_identical(snapshot(file.path(root, "two")), before)
  unlink(shadow, recursive = TRUE)
  third <- reflow_artifact_plan_run(
    make("changed"), file.path(root, "three"),
    file.path(root, "two")
  )
  expect_identical(third$nodes, second$nodes)
  unlink(ready_path)
  expect_error(reflow_artifact_plan_resume(make("changed"), file.path(root, "three")))
  writeBin(ready_bytes, ready_path)

  # Changed tracked content invalidates only its dependency branch.
  input <- file.path(root, "input.txt")
  writeLines("old", input)
  input_plan <- reflow_artifact_plan(
    graph_stage("a", list(input = reflow_imaging_input(input))),
    graph_stage("b", list(input = reflow_artifact_ref("a", "value"))),
    graph_stage("unrelated", list(value = "stable"))
  )
  input_one <- reflow_artifact_plan_run(input_plan, file.path(root, "input_one"))
  writeLines("new", input)
  expect_error(
    reflow_artifact_plan_resume(input_plan, file.path(root, "input_one")),
    "changed"
  )
  input_two <- reflow_artifact_plan_run(
    input_plan, file.path(root, "input_two"),
    file.path(root, "input_one")
  )
  expect_identical(text(input_two$nodes$b), "new alpha alpha")
  expect_identical(input_two$nodes$unrelated, input_one$nodes$unrelated)
  expect_false(identical(input_two$nodes$a, input_one$nodes$a))
  expect_false(identical(input_two$nodes$b, input_one$nodes$b))

  object_stage <- reflow_artifact_stage("object", "graphfixture", "write_object",
    outputs = list(path = list(path = "object.rds", type = "file")),
    inventory = c(object = "object.rds")
  )
  object_plan <- reflow_artifact_plan(object_stage, graph_stage("consumer", list(
    object = reflow_artifact_ref("object", "object", read = TRUE)
  )))
  object_run <- reflow_artifact_plan_run(object_plan, file.path(root, "object"))
  expect_identical(text(object_run$nodes$consumer), "red blue alpha")

  failed_plan <- reflow_artifact_plan(graph_stage("retry", list(fail_first = TRUE)))
  failed <- file.path(root, "failed")
  expect_error(reflow_artifact_plan_run(failed_plan, failed), "deliberately")
  before <- snapshot(failed)
  expect_error(reflow_artifact_plan_resume(failed_plan, failed), "reconciliation")
  expect_identical(snapshot(failed), before)
  state <- readRDS(file.path(failed, "retry", "state.rds"))
  retry <- reflow_artifact_plan_resume(failed_plan, failed, list(retry = list(
    reconciled_attempt = state$attempt,
    reconciliation = "Synchronous fixture has exited; no children exist."
  )))
  expect_identical(text(retry$nodes$retry), "alpha")
  expect_length(list.files(file.path(failed, "retry"), pattern = "^attempt-"), 2L)

  # Corrupted persistence is refused without dispatch or evidence repair.
  mutate_rds <- function(path, change, action, pattern) {
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    on.exit(writeBin(bytes, path), add = TRUE)
    saveRDS(change(readRDS(path)), path)
    before <- snapshot(d1)
    expect_error(action(), pattern)
    expect_identical(snapshot(d1), before)
    expect_false(dir.exists(file.path(d1, ".lock")))
  }
  resume_first <- function() reflow_artifact_plan_resume(p, d1)
  journal <- file.path(d1, "nodes.rds")
  mutate_rds(journal, function(x) 1L, resume_first, "Invalid node journal")
  mutate_rds(journal, rev, resume_first, "execution prefix")
  mutate_rds(journal, function(x) list(), resume_first, "undispatched")
  mutate_rds(journal, function(x) {
    x$a$dependencies <- NULL
    x
  }, resume_first, "Malformed node record")
  mutate_rds(journal, function(x) {
    x$a$definition$runtime <- c(changed = "resource")
    x
  }, resume_first, "Node definition differs")
  mutate_rds(journal, function(x) {
    x$a$descriptor$bundle <- "wrong-location"
    x
  }, resume_first, "Node descriptor differs")
  mutate_rds(journal, function(x) {
    x$a$dependencies <- list(foreign = "producer")
    x
  }, resume_first, "Producer lineage differs")
  mutate_rds(file.path(d1, "READY.rds"), function(x) {
    x$definition_hash <- "corrupt"
    x
  }, resume_first, "Graph READY differs")

  local_node <- file.path(d1, "a")
  moved <- file.path(root, "held-a")
  expect_true(file.rename(local_node, moved))
  writeLines("not a directory", local_node)
  before <- snapshot(d1)
  expect_error(resume_first(), "Stage path must be a directory")
  expect_identical(snapshot(d1), before)
  unlink(local_node)
  expect_true(file.rename(moved, local_node))

  new_from_prior <- function() {
    destination <- file.path(root, "refused-new")
    on.exit(expect_false(dir.exists(destination)), add = TRUE)
    reflow_artifact_plan_run(p, destination, d1)
  }
  mutate_rds(file.path(d1, "previous.rds"), function(x) {
    list(directory = root, ready_hash = paste(rep("0", 64), collapse = ""))
  }, new_from_prior, "Previous binding differs")
  definition_path <- file.path(d1, "definition.rds")
  mutate_rds(definition_path, function(x) {
    x$schema <- "unsupported"
    x
  }, new_from_prior, "Malformed historical graph")
  mutate_rds(definition_path, function(x) {
    x$signatures$a$schema <- "unsupported"
    x
  }, new_from_prior, "Malformed historical stage")
  mutate_rds(definition_path, function(x) {
    x["previous"] <- list(list(unexpected = "field"))
    x
  }, new_from_prior, "Malformed previous binding")
  mutate_rds(definition_path, function(x) {
    x["previous"] <- list(list(directory = root, ready_hash = "not-a-hash"))
    x
  }, new_from_prior, "Invalid prior hash")

  # A coherent but cyclic saved-generation link cannot schedule itself.
  saved_files <- file.path(d1, c("definition.rds", "previous.rds", "READY.rds"))
  saved_bytes <- lapply(saved_files, function(f) readBin(f, "raw", file.info(f)$size))
  cyclic <- readRDS(definition_path)
  cyclic$previous <- list(directory = d1, ready_hash = paste(rep("0", 64), collapse = ""))
  saveRDS(cyclic, definition_path)
  saveRDS(cyclic$previous, saved_files[2])
  ready <- readRDS(saved_files[3])
  ready$definition_hash <- rf_hash(cyclic)
  saveRDS(ready, saved_files[3])
  before <- snapshot(d1)
  expect_error(new_from_prior(), "Cyclic or overlapping history")
  expect_identical(snapshot(d1), before)
  for (i in seq_along(saved_files)) writeBin(saved_bytes[[i]], saved_files[i])

  boundary_before <- snapshot(d1)
  expect_error(reflow_artifact_plan_run(p, file.path(root, "absent", "child")), "parent")
  expect_error(reflow_artifact_plan_resume(p, file.path(root, "absent")), "missing")
  expect_error(reflow_artifact_plan_resume(p, d1, list(foreign = list())), "reconciliations")
  expect_error(reflow_artifact_plan_resume(p, d1, list(a = list(wrong = TRUE))), "fields")
  expect_error(reflow_artifact_plan_run(p, d1), "Fresh generation")
  expect_error(reflow_artifact_plan_run(p, file.path(d1, "nested"), d1), "overlap")
  input_directory <- file.path(root, "tracked-directory")
  dir.create(input_directory)
  dir_plan <- reflow_artifact_plan(
    graph_stage("a", list(input = reflow_imaging_input(input_directory)))
  )
  expect_error(reflow_artifact_plan_run(dir_plan, file.path(input_directory, "nested")), "overlap")
  expect_length(list.files(input_directory, all.files = TRUE, no.. = TRUE), 0L)
  malformed <- p
  attr(malformed, "extra") <- TRUE
  expect_error(reflow_artifact_plan_run(malformed, file.path(root, "bad-plan")), "Noncanonical")
  expect_false(dir.exists(file.path(root, "bad-plan")))
  expect_false(dir.exists(file.path(root, "absent")))
  expect_false(dir.exists(file.path(d1, "nested")))
  expect_identical(snapshot(d1), boundary_before)

  fault <- function(bindings, code) {
    do.call(
      testthat::local_mocked_bindings,
      c(bindings, list(.package = "reflowR", .env = environment()))
    )
    force(code)
  }
  original_write <- reflowR:::rf_write
  owned <- file.path(root, "owner-write-failure")
  fault(list(rf_write = function(x, path) {
    if (basename(path) == "owner.rds") stop("owned storage write failed")
    original_write(x, path)
  }), {
    expect_error(reflow_artifact_plan_run(p, owned), "storage write failed")
    expect_length(list.files(owned, all.files = TRUE, no.. = TRUE), 0L)
  })

  # Simulate loss of the locked directory; the externally moved files survive.
  original_definition <- reflowR:::rfg_definition
  moved_graph <- file.path(root, "externally-moved")
  relative_snapshot <- function(path) {
    result <- snapshot(path)
    names(result) <- substring(names(result), nchar(path) + 2L)
    result
  }
  before <- relative_snapshot(d1)
  fault(list(rfg_definition = function(plan) {
    answer <- original_definition(plan)
    stopifnot(file.rename(d1, moved_graph))
    answer
  }), expect_error(resume_first(), "Generation directory missing"))
  unlink(file.path(moved_graph, ".lock"), recursive = TRUE)
  expect_identical(relative_snapshot(moved_graph), before)
  expect_true(file.rename(moved_graph, d1))

  # A real input change between graph preflight and child reuse is refused.
  input_bytes <- readBin(input, "raw", file.info(input)$size)
  original_records <- reflowR:::rfg_records
  before <- snapshot(file.path(root, "input_two"))
  fault(list(rfg_records = function(...) {
    answer <- original_records(...)
    writeLines("concurrently changed", input)
    answer
  }), expect_error(
    reflow_artifact_plan_resume(input_plan, file.path(root, "input_two")),
    "Completed node inputs or runtime changed"
  ))
  expect_identical(snapshot(file.path(root, "input_two")), before)
  writeBin(input_bytes, input)

  # A graph never publishes READY if its input changes at final verification.
  calls <- 0L
  final_dir <- file.path(root, "changed-at-final-verification")
  final_plan <- reflow_artifact_plan(graph_stage("a", list(input = reflow_imaging_input(input))))
  fault(list(rfg_definition = function(plan) {
    calls <<- calls + 1L
    if (calls == 2L) writeLines("changed after child completion", input)
    original_definition(plan)
  }), expect_error(reflow_artifact_plan_run(final_plan, final_dir), "Graph changed during run"))
  expect_false(file.exists(file.path(final_dir, "READY.rds")))
  expect_true(file.exists(file.path(final_dir, "a", "READY.rds")))
  expect_length(list.files(file.path(final_dir, "a"), pattern = "^attempt-"), 1L)
  writeBin(input_bytes, input)

  # Actual competing invocations: the first remains inside its installed writer.
  if (.Platform$OS.type == "unix") {
    slow <- reflow_artifact_plan(graph_stage("slow", list(delay = 2)))
    destination <- file.path(root, "concurrent")
    job <- parallel::mcparallel(reflow_artifact_plan_run(slow, destination),
      mc.set.seed = FALSE
    )
    on.exit(if (!is.null(job)) parallel::mccollect(job), add = TRUE)
    deadline <- Sys.time() + 30
    while (!dir.exists(file.path(destination, ".lock")) && Sys.time() < deadline) {
      Sys.sleep(0.05)
    }
    expect_true(dir.exists(file.path(destination, ".lock")))
    expect_error(reflow_artifact_plan_resume(slow, destination), "locked")
    result <- parallel::mccollect(job)
    job <- NULL
    expect_false(inherits(result[[1]], "try-error"))
    expect_identical(result[[1]]$schema, "reflow_artifact_graph_descriptor_1")
    expect_length(list.files(file.path(destination, "slow"), pattern = "^attempt-"), 1L)
  }
})


test_that("malformed declarations are refused before any execution", {
  expect_error(reflow_artifact_plan(), "at least one")
  expect_error(reflow_artifact_plan(list()), "Invalid deferred")
  stage <- graph_stage("a")
  attr(stage, "extra") <- TRUE
  expect_error(reflow_artifact_plan(stage), "Noncanonical deferred")
  stage <- graph_stage("a")
  stage$declaration$schema <- "unknown"
  expect_error(reflow_artifact_plan(stage), "Invalid declaration")
  reference <- reflow_artifact_ref("a", "value")
  attr(reference, "extra") <- TRUE
  expect_error(graph_stage("b", list(input = reference)), "Noncanonical artifact reference")
  expect_error(graph_stage("a", list(input = new.env())), "Unsupported argument")
})
