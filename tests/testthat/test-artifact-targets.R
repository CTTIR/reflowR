test_that("targets caches only a reference and makes always reverify", {
  skip_if_not_installed("targets")
  skip_if_not(identical(Sys.info()[["sysname"]], "Linux"))
  skip_on_cran()
  f <- selector_fixture()
  withr::local_dir(f$root)
  command <- quote(reflowR::reflow_artifact_plan(
    reflowR::reflow_artifact_stage("literal", "base", "writeLines",
      list(text = "anonymous literal"),
      outputs = list(con = list(path = "value.txt", type = "file")),
      inventory = c(value = "value.txt"))))
  plan <- eval(command)
  p <- reflow_artifact_select(plan, f$registry, initialize = TRUE)
  script <- substitute(list(reflowR::reflow_artifact_target(
    "selected", quote(COMMAND), REGISTRY)),
    list(COMMAND = command, REGISTRY = f$registry))
  writeLines(deparse(script, width.cutoff = 500L), "_targets.R")
  count <- 0L
  original <- reflow_artifact_select
  local_mocked_bindings(reflow_artifact_select = function(...) {
    count <<- count + 1L
    original(...)
  }, .package = "reflowR")
  targets::tar_make(callr_function = NULL, reporter = "silent")
  first <- targets::tar_read(selected)
  expect_identical(first, p)
  before <- selector_snapshot(p$generation)
  targets::tar_make(callr_function = NULL, reporter = "silent")
  expect_identical(count, 2L)
  expect_identical(targets::tar_read(selected), p)
  expect_identical(selector_snapshot(p$generation), before)
  expect_identical(names(first), c("schema", "registry", "event", "event_hash",
    "generation", "request_hash", "ready_hash", "status"))
  graph <- reflow_artifact_selection_verify(first, plan)
  writeLines("corrupt", file.path(graph$nodes$literal$descriptor$bundle, "value.txt"))
  expect_identical(targets::tar_read(selected)$status, "REFERENCE_REQUIRES_VERIFICATION")
  expect_error(reflow_artifact_selection_verify(first, plan))
  expect_error(targets::tar_make(callr_function = NULL, reporter = "silent"))
})

test_that("target declaration rejects evaluated plans and strings", {
  f <- selector_fixture()
  expect_error(reflow_artifact_target("selected", "make_plan()", f$registry), "language")
  expect_error(reflow_artifact_target("selected", f$make(), f$registry), "language")
  expect_error(reflow_artifact_target("bad name", quote(make_plan()), f$registry), "name")
})
