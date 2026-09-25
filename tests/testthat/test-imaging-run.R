image_plan <- function(x = 1:4) {
  reflow_imaging_plan(
    reflow_imaging_stage("summary", "base", "summary", list(object = x)),
    reflow_imaging_stage("mean", "base", "mean", list(x = reflow_imaging_ref("summary")))
  )
}

test_that("explicit dependency calls persist and resume without rewriting outputs", {
  path <- file.path(withr::local_tempdir(), "run with spaces")
  plan <- image_plan()
  out <- reflow_imaging_run(plan, path)
  expect_equal(unname(out$mean), 2.5)
  expect_true(file.exists(file.path(path, "READY")))
  receipt <- readRDS(file.path(path, "mean.rds"))
  expect_identical(receipt$status, "READY")
  expect_true(all(nzchar(unlist(receipt[c(
    "input_hash", "config_hash", "package_hash",
    "runtime_hash", "output_hash"
  )]))))
  before <- list.files(path, recursive = TRUE)
  expect_identical(reflow_imaging_resume(plan, path), out)
  expect_identical(list.files(path, recursive = TRUE), before)
  expect_error(reflow_imaging_run(plan, path), "already exists")
  expect_error(reflow_imaging_resume(image_plan(2:5), path), "changed")
  expect_false(file.exists(file.path(path, "READY")))
  expect_identical(readRDS(file.path(path, "state.rds"))$status, "FAILED")
})

test_that("tracked input changes and corrupt caches cannot be ready", {
  root <- withr::local_tempdir()
  src <- file.path(root, "input.rds")
  saveRDS(1:3, src)
  plan <- reflow_imaging_plan(reflow_imaging_stage(
    "mean", "base", "mean",
    list(x = reflow_imaging_input(src, read = TRUE))
  ))
  path <- file.path(root, "run")
  expect_equal(reflow_imaging_run(plan, path)$mean, 2)
  saveRDS(2:4, src)
  expect_error(reflow_imaging_resume(plan, path), "changed")
  expect_false(file.exists(file.path(path, "READY")))
  saveRDS(1:3, src)
  receipt <- readRDS(file.path(path, "mean.rds"))
  writeLines("broken", file.path(path, receipt$output))
  expect_error(reflow_imaging_resume(plan, path), "Corrupt")
  expect_false(file.exists(file.path(path, "READY")))
})

test_that("failed calls retry and retain successful upstream artifacts", {
  root <- withr::local_tempdir()
  path <- file.path(root, "run")
  plan <- reflow_imaging_plan(
    reflow_imaging_stage("ok", "base", "sum", list(... = 1:3)),
    reflow_imaging_stage("fail", "base", "stop", list(... = "deliberate failure"))
  )
  expect_error(reflow_imaging_run(plan, path), "deliberate failure")
  first <- readRDS(file.path(path, "ok.rds"))
  expect_identical(first$status, "READY")
  expect_identical(readRDS(file.path(path, "fail.rds"))$status, "FAILED")
  expect_error(reflow_imaging_resume(plan, path), "deliberate failure")
  expect_identical(readRDS(file.path(path, "ok.rds")), first)
  expect_false(file.exists(file.path(path, "READY")))
  expect_length(list.dirs(path, recursive = FALSE), 3L)
})

test_that("invalid definitions fail before output creation and locks fail closed", {
  root <- withr::local_tempdir()
  path <- file.path(root, "run")
  expect_error(reflow_imaging_stage("../bad", "base", "sum"), "Invalid")
  expect_error(reflow_imaging_plan(reflow_imaging_stage(
    "a", "base", "mean",
    list(x = reflow_imaging_ref("b"))
  )), "earlier")
  bad <- reflow_imaging_plan(reflow_imaging_stage("a", "base", "not_an_export"))
  expect_error(reflow_imaging_run(bad, path))
  expect_false(dir.exists(path))
  expect_error(reflow_imaging_plan(reflow_imaging_stage(
    "a", "base", "mean",
    list(x = function() 1)
  )), "unsupported")
  reflow_imaging_run(image_plan(), path)
  dir.create(file.path(path, ".lock"))
  expect_error(reflow_imaging_resume(image_plan(), path), "locked")
  expect_true(dir.exists(file.path(path, ".lock")))
})

test_that("interrupted stage retry uses a new attempt and verifies its dependencies", {
  path <- file.path(withr::local_tempdir(), "run")
  plan <- image_plan()
  out <- reflow_imaging_run(plan, path)
  upstream <- readRDS(file.path(path, "summary.rds"))
  interrupted <- readRDS(file.path(path, "mean.rds"))
  interrupted$status <- "RUNNING"
  saveRDS(interrupted, file.path(path, "mean.rds"))
  unlink(file.path(path, "READY"))
  expect_identical(reflow_imaging_resume(plan, path), out)
  expect_identical(readRDS(file.path(path, "summary.rds")), upstream)
  expect_false(identical(readRDS(file.path(path, "mean.rds"))$output, interrupted$output))
  expect_true(file.exists(file.path(path, interrupted$output)))
})

test_that("runtime and package changes invalidate resume independently", {
  path <- file.path(withr::local_tempdir(), "run")
  plan <- image_plan()
  reflow_imaging_run(plan, path)
  original <- rf_signature
  local_mocked_bindings(rf_signature = function(plan) {
    x <- original(plan)
    x$runtime_hash <- "different runtime"
    x
  })
  expect_error(reflow_imaging_resume(plan, path), "changed")
  local_mocked_bindings(rf_signature = function(plan) {
    x <- original(plan)
    x$package_hash <- "different installed code"
    x
  })
  expect_error(reflow_imaging_resume(plan, path), "changed")
})

test_that("component references, null results and directory inputs are explicit", {
  root <- withr::local_tempdir()
  source <- file.path(root, "source")
  dir.create(source)
  writeLines("x", file.path(source, "x.txt"))
  plan <- reflow_imaging_plan(
    reflow_imaging_stage("list", "base", "list", list(values = 1:4)),
    reflow_imaging_stage("mean", "base", "mean", list(x = reflow_imaging_ref("list", "values"))),
    reflow_imaging_stage("null", "base", "identity", list(x = NULL)),
    reflow_imaging_stage("dir", "base", "list.files", list(path = reflow_imaging_input(source)))
  )
  path <- file.path(root, "run")
  out <- reflow_imaging_run(plan, path)
  expect_equal(out$mean, 2.5)
  expect_named(out, c("list", "mean", "null", "dir"))
  expect_null(out$null)
  expect_identical(out$dir, "x.txt")
  writeLines("y", file.path(source, "y.txt"))
  expect_error(reflow_imaging_resume(plan, path), "changed")
})

test_that("malformed receipts are terminal invalid and cannot trigger silent recomputation", {
  path <- file.path(withr::local_tempdir(), "run")
  reflow_imaging_run(image_plan(), path)
  r <- readRDS(file.path(path, "summary.rds"))
  r$output <- "../outside.rds"
  saveRDS(r, file.path(path, "summary.rds"))
  expect_error(reflow_imaging_resume(image_plan(), path), "Invalid stage receipt")
  expect_identical(readRDS(file.path(path, "summary.rds"))$status, "INVALID")
  expect_error(reflow_imaging_resume(image_plan(), path), "Invalid stage cache")
})

test_that("a caught interrupt leaves durable failure and releases lock", {
  path <- file.path(withr::local_tempdir(), "run")
  interrupt <- structure(list(message = "interrupted", call = NULL),
                         class = c("interrupt", "condition"))
  plan <- reflow_imaging_plan(
    reflow_imaging_stage("interrupt", "base", "stop", list(... = interrupt))
  )
  caught <- tryCatch(reflow_imaging_run(plan, path), interrupt = function(e) TRUE)
  expect_true(caught)
  expect_identical(readRDS(file.path(path, "interrupt.rds"))$status, "FAILED")
  expect_identical(readRDS(file.path(path, "state.rds"))$status, "FAILED")
  expect_false(file.exists(file.path(path, "READY")))
  expect_false(dir.exists(file.path(path, ".lock")))
})

test_that("missing optional packages fail before execution", {
  path <- file.path(withr::local_tempdir(), "run")
  plan <- reflow_imaging_plan(reflow_imaging_stage("x", "base", "identity", list(x = 1)),
    packages = "notARealReflowPackage"
  )
  expect_error(reflow_imaging_run(plan, path), "Missing packages")
  expect_false(dir.exists(path))
})

test_that("malformed declarations and missing tracked inputs fail early", {
  expect_error(reflow_imaging_stage(NA_character_, "base", "sum"), "strings")
  expect_error(reflow_imaging_stage("a", "base", "sum", list(1)), "named")
  expect_error(reflow_imaging_ref(NA_character_), "Invalid")
  expect_error(reflow_imaging_input(1), "Invalid")
  expect_error(reflow_imaging_input(tempdir(), read = TRUE), "file")
  expect_error(reflow_imaging_plan(packages = NA_character_), "names")
  expect_error(reflow_imaging_plan(), "at least")
  stage <- reflow_imaging_stage("a", "base", "identity", list(x = 1))
  expect_error(reflow_imaging_plan(stage, stage), "Duplicate")
  path <- file.path(withr::local_tempdir(), "run")
  expect_error(reflow_imaging_resume(reflow_imaging_plan(stage), path), "Missing run")
  expect_error(reflow_imaging_run(list(schema = "bad"), path), "schema")
  bad <- reflow_imaging_plan(reflow_imaging_stage("a", "base", "pi"))
  expect_error(reflow_imaging_run(bad, path), "not a function")
  src <- tempfile()
  writeLines("x", src)
  input <- reflow_imaging_input(src)
  unlink(src)
  expect_error(reflow_imaging_run(reflow_imaging_plan(
    reflow_imaging_stage("a", "base", "identity", list(x = input))
  ), path), "Missing tracked")
})

test_that("input mutation inside a stage is a recorded failure", {
  root <- withr::local_tempdir()
  src <- file.path(root, "source.txt")
  writeLines("before", src)
  path <- file.path(root, "run")
  plan <- reflow_imaging_plan(reflow_imaging_stage(
    "mutate", "base", "writeLines",
    list(text = "after", con = reflow_imaging_input(src))
  ))
  expect_error(reflow_imaging_run(plan, path), "changed during execution")
  expect_identical(readRDS(file.path(path, "mutate.rds"))$status, "FAILED")
  expect_false(file.exists(file.path(path, "READY")))
})

test_that("missing dependency components fail rather than silently supplying NULL", {
  path <- file.path(withr::local_tempdir(), "run")
  plan <- reflow_imaging_plan(
    reflow_imaging_stage("a", "base", "list", list(x = 1)),
    reflow_imaging_stage("b", "base", "identity", list(x = reflow_imaging_ref("a", "missing")))
  )
  expect_error(reflow_imaging_run(plan, path), "Missing reference component")
  expect_false(file.exists(file.path(path, "READY")))
})

test_that("stochastic stages require recorded seeds and restore caller RNG", {
  root <- withr::local_tempdir()
  withr::local_seed(71)
  before <- .Random.seed
  unseeded <- reflow_imaging_plan(
    reflow_imaging_stage("random", "stats", "rnorm", list(n = 5))
  )
  expect_error(reflow_imaging_run(unseeded, file.path(root, "bad")), "explicit seed")
  expect_identical(.Random.seed, before)
  seeded <- reflow_imaging_plan(
    reflow_imaging_stage("random", "stats", "rnorm", list(n = 5), seed = 19)
  )
  a <- reflow_imaging_run(seeded, file.path(root, "a"))
  b <- reflow_imaging_run(seeded, file.path(root, "b"))
  expect_identical(a, b)
  expect_identical(.Random.seed, before)
  expect_error(reflow_imaging_stage("a", "stats", "rnorm", seed = 1.5), "integer")
})


test_that("numerical thread environment changes invalidate resume", {
  withr::local_envvar(c(OPENBLAS_NUM_THREADS = "1", OMP_NUM_THREADS = NA))
  path <- file.path(withr::local_tempdir(), "thread-run")
  plan <- image_plan()
  original <- reflow_imaging_run(plan, path)
  definition <- readRDS(file.path(path, "definition.rds"))
  expect_identical(definition$numerical_environment[["OPENBLAS_NUM_THREADS"]], "1")
  expect_identical(definition$numerical_environment[["OMP_NUM_THREADS"]], NA_character_)
  withr::with_envvar(c(OPENBLAS_NUM_THREADS = "2"), {
    expect_error(reflow_imaging_resume(plan, path), "changed")
    expect_false(file.exists(file.path(path, "READY")))
  })
  expect_identical(reflow_imaging_resume(plan, path), original)
})
