# Predicate extraction never runs the guardian top level or starts a process.
deadline_find <- function(x, predicate) {
  found <- list()
  walk <- function(node) {
    if (is.call(node) && predicate(node)) found[[length(found) + 1L]] <<- node
    if (is.call(node) || is.expression(node)) {
      for (child in as.list(node)) walk(child)
    }
  }
  walk(x)
  stopifnot(length(found) == 1L)
  found[[1L]]
}
deadline_resources <- function(seconds) {
  list(timeout_seconds = seconds, address_space_bytes = 1024^3,
    nice = 10L, threads = 2L, temp_directory = "/anonymous/tmp",
    cache_directory = "/anonymous/cache", rscript = "/r",
    prlimit = "/p", nice_command = "/n")
}

test_that("explicit finite deadline boundaries agree with independent guardian", {
  helper <- new.env(parent = baseenv())
  sys.source(system.file("bounded", "executor.R", package = "reflowR",
    mustWork = TRUE), helper)
  guardian <- parse(system.file("bounded", "guardian.R", package = "reflowR",
    mustWork = TRUE))
  predicate <- deadline_find(guardian, function(x) {
    identical(x[[1L]], as.name("rfx_number")) &&
      identical(x[[2L]], quote(d$deadline_seconds))
  })
  for (seconds in list(1L, 1, 60, 61, 3600, 86399.5, 86400L, 86400)) {
    resource <- deadline_resources(seconds)
    expect_identical(rfb_resources(resource), resource)
    helper$d <- list(deadline_seconds = seconds)
    expect_true(eval(predicate, helper))
  }
  invalid <- list(0, -1, 0.999, 86400.001, 86401, Inf, -Inf, NA_real_,
    NaN, NULL, numeric(), c(1, 2), TRUE, "61", 1i, matrix(61),
    structure(61, class = "deadline"))
  for (seconds in invalid) {
    expect_error(rfb_resources(deadline_resources(seconds)),
      "Invalid bounded resource limits.", fixed = TRUE)
    helper$d <- list(deadline_seconds = seconds)
    expect_false(eval(predicate, helper))
  }
})

test_that("simulated elapsed setup consumes the declared guardian budget", {
  guardian <- parse(system.file("bounded", "guardian.R", package = "reflowR",
    mustWork = TRUE))
  start <- deadline_find(guardian, function(x) {
    identical(x[[1L]], as.name("<-")) && identical(x[[2L]], as.name("deadline"))
  })
  remaining <- deadline_find(guardian, function(x) {
    identical(x[[1L]], as.name("max")) && length(x) == 3L &&
      identical(x[[2L]], 0.01)
  })
  clock <- new.env(parent = baseenv())
  clock$now <- 100
  clock$proc.time <- function() c(elapsed = clock$now)
  clock$d <- list(deadline_seconds = 3600)
  eval(start, clock)
  expect_identical(clock$deadline, 3700)
  clock$now <- 160
  expect_identical(eval(remaining, clock), 3540)
  clock$now <- 3699.5
  expect_identical(eval(remaining, clock), 0.5)
  clock$now <- 3701
  expect_identical(eval(remaining, clock), 0.01)
  # The arithmetic floor does not imply permission to dispatch after expiry.
  expired <- deadline_find(guardian, function(x) {
    identical(x[[1L]], as.name("<-")) && identical(x[[2L]], as.name("check"))
  })
  clock$identity_alive <- function() TRUE
  clock$rg_receive <- function(...) stop("expired budget reached transport")
  clock$reason <- NULL
  eval(expired, clock)
  expect_true(clock$check())
  expect_identical(clock$reason, "DEADLINE")
})

test_that("an invalid later stage refuses before graph or control allocation", {
  skip_if(Sys.info()[["sysname"]] != "Linux", "Linux canonical path contract")
  root <- tempfile(tmpdir = normalizePath(tempdir(), mustWork = TRUE))
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  graph <- file.path(root, "graph")
  control <- file.path(root, "control")
  plan <- list(stages = list(a = list(declaration = list(args = list())),
    b = list(declaration = list(args = list()))))
  resources <- list(a = deadline_resources(61), b = deadline_resources(86401))
  checked <- numeric()
  local_mocked_bindings(
    rfg_definition = function(plan) list(signature = "anonymous"),
    rfb_preflight = function(spec, resources, runtime_files) {
      checked <<- c(checked, resources$timeout_seconds)
      list(resources = rfb_resources(resources))
    }, .package = "reflowR")
  expect_error(rfbg_execute(plan, graph, NULL, TRUE, list(), control,
    resources, character(), function() FALSE), "Invalid bounded resource limits.",
    fixed = TRUE)
  expect_identical(checked, c(61, 86401))
  expect_false(file.exists(graph))
  expect_false(file.exists(control))
  expect_identical(list.files(root, all.files = TRUE, no.. = TRUE), character())
})
