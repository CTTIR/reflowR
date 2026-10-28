test_that("the call writer saves the exact value and a readable table", {
  dir <- withr::local_tempdir()
  value <- data.frame(
    id = c("a", "b\tc", NA), x = c(1.5, NA, 1 / 3), flag = c(TRUE, FALSE, NA),
    level = factor(c("u", "v", "u")),
    text = c("say \"hi\"", "line\nbreak", "plain"), stringsAsFactors = FALSE
  )
  rds <- file.path(dir, "value.rds")
  tsv <- file.path(dir, "value.tsv")
  out <- reflow_call_writer("base::identity", list(x = value), rds, tsv)
  expect_identical(out, c(rds, tsv))
  expect_identical(readRDS(rds), value)
  back <- utils::read.delim(tsv, stringsAsFactors = FALSE, na.strings = "NA")
  expect_identical(back$id, value$id)
  expect_equal(back$x, value$x, tolerance = 1e-14)
  expect_identical(back$flag, value$flag)
  expect_identical(back$level, as.character(value$level))
  expect_identical(back$text, value$text)

  only <- file.path(dir, "only.rds")
  expect_identical(reflow_call_writer("utils::head", list(x = 1:5, n = 2), only), only)
  expect_identical(readRDS(only), 1:2)
  expect_false(file.exists(file.path(dir, "only.tsv")))
})

test_that("values that are not plain tables give an empty table file", {
  dir <- withr::local_tempdir()
  rds <- file.path(dir, "value.rds")
  tsv <- file.path(dir, "value.tsv")
  reflow_call_writer("base::list", list(a = 1, b = "x"), rds, tsv)
  expect_identical(readRDS(rds), list(a = 1, b = "x"))
  expect_identical(file.size(tsv), 0)

  nested <- data.frame(id = 1:2)
  nested$items <- list(1, 2:3)
  reflow_call_writer("base::identity", list(x = nested), rds, tsv)
  expect_identical(readRDS(rds), nested)
  expect_identical(file.size(tsv), 0)

  reflow_call_writer("base::identity", list(x = data.frame(k = character())), rds, tsv)
  expect_identical(readLines(tsv), "k")
})

test_that("the call writer refuses malformed calls before writing", {
  dir <- withr::local_tempdir()
  rds <- file.path(dir, "value.rds")
  for (fun in list("base:::identity", "identity", "::identity", "base::",
                   c("base::identity", "base::list"), NA_character_, 1)) {
    expect_error(reflow_call_writer(fun, list(x = 1), rds))
  }
  expect_error(reflow_call_writer("stats::not_a_function_here", list(), rds), "exported")
  expect_error(reflow_call_writer("base::identity", list(1), rds), "names")
  expect_error(reflow_call_writer("base::identity", list(x = 1, x = 2), rds), "names")
  expect_error(reflow_call_writer("base::identity", "x", rds), "plain list")
  expect_error(reflow_call_writer("base::identity", list(x = 1), rds, rds), "differ")
  expect_error(reflow_call_writer("base::identity", list(x = 1), ""), "plain text")
  expect_false(file.exists(rds))
})

test_that("call stages are canonical artifact declarations", {
  a <- reflow_call_stage("a", "utils::head", list(x = 1:3, n = 1))
  expect_s3_class(a, "reflow_artifact_stage")
  d <- a$declaration
  expect_identical(d$package, "reflowR")
  expect_identical(d$fun, "reflow_call_writer")
  expect_identical(d$args, list(fun = "utils::head", args = list(x = 1:3, n = 1)))
  expect_identical(d$inventory, c(value = "value.rds", table = "value.tsv"))
  expect_identical(d$allow_empty, "table")
  expect_identical(d$packages, "utils")
  b <- reflow_call_stage("b", "stats::median",
    list(x = reflow_artifact_ref("a", "value", read = TRUE)),
    packages = c("utils", "stats")
  )
  expect_identical(b$declaration$packages, c("stats", "utils"))
  expect_identical(reflow_artifact_plan(a, b), reflow_artifact_plan(b, a))
  expect_identical(names(reflow_artifact_plan(b, a)$stages), c("a", "b"))
  expect_error(reflow_call_stage("a", "utils:::head"), "package::function")
  expect_error(reflow_call_stage("bad-id", "utils::head"), "Invalid")
  expect_error(reflow_call_stage("a", "utils::head", packages = NA_character_), "packages")
  expect_error(reflow_call_stage("a", "utils::head", seed = -1), "seed")
  expect_error(reflow_call_stage("a", "utils::head", list(1)), "names")
})

test_that("call stages run, resume and fingerprint as an artifact graph", {
  skip_if_not(
    "reflowR" %in% rownames(utils::installed.packages()),
    "needs an installed reflowR"
  )
  root <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
  root <- tempfile("call graph ", root)
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  input <- file.path(root, "input.tsv")
  writeLines(c("k\tv", "a\t1", "b\t2", "c\t3"), input)
  plan <- reflow_artifact_plan(
    reflow_call_stage("count", "base::nrow", list(
      x = reflow_artifact_ref("top", "value", read = TRUE)
    )),
    reflow_call_stage("top", "utils::head", list(
      x = reflow_artifact_ref("table", "value", read = TRUE), n = 2
    )),
    reflow_call_stage("table", "utils::read.delim", list(
      file = reflow_imaging_input(input), stringsAsFactors = FALSE
    )),
    reflow_call_stage("draw", "stats::runif", list(n = 2), seed = 7)
  )
  expect_identical(names(plan$stages), c("draw", "table", "top", "count"))
  directory <- file.path(root, "one")
  run <- reflow_artifact_plan_run(plan, directory)
  bundle <- function(id) run$nodes[[id]]$descriptor$bundle
  value <- function(id) readRDS(file.path(bundle(id), "value.rds"))
  expected <- utils::head(utils::read.delim(input, stringsAsFactors = FALSE), 2)
  expect_identical(value("top"), expected)
  expect_identical(readLines(file.path(bundle("top"), "value.tsv")), c("k\tv", "a\t1", "b\t2"))
  expect_identical(value("draw"), withr::with_seed(7, stats::runif(2)))
  expect_identical(value("count"), 2L)
  expect_identical(file.size(file.path(bundle("count"), "value.tsv")), 0)
  expect_identical(reflow_artifact_plan_resume(plan, directory), run)

  noise <- reflow_artifact_plan(reflow_call_stage("noise", "stats::runif", list(n = 1)))
  expect_error(reflow_artifact_plan_run(noise, file.path(root, "noise")), "explicit seed")

  writeLines(c("k\tv", "z\t9"), input)
  expect_error(reflow_artifact_plan_resume(plan, directory), "changed")
  second <- reflow_artifact_plan_run(plan, file.path(root, "two"), directory)
  expect_identical(second$nodes$draw, run$nodes$draw)
  expect_false(identical(second$nodes$table, run$nodes$table))
  expect_identical(
    readRDS(file.path(second$nodes$count$descriptor$bundle, "value.rds")), 1L
  )
})
