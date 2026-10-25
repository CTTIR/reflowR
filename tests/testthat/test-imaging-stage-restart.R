restart_plan <- function(direct = FALSE) {
  warning_message <- "stage protocol warning"
  producer <- if (direct) {
    reflow_imaging_stage("producer", "base", "signalCondition",
                         list(cond = simpleWarning(warning_message)), seed = 19)
  } else {
    reflow_imaging_stage("producer", "base", "warning",
                         list(... = warning_message), seed = 19)
  }
  reflow_imaging_plan(
    producer,
    reflow_imaging_stage("consumer", "base", "identity",
                         list(x = reflow_imaging_ref("producer")))
  )
}

expect_stage_aborted <- function(path, error, expected_message = "stage protocol warning") {
  expect_s3_class(error, "simpleError")
  expect_false(inherits(error, "warning"))
  expect_identical(conditionMessage(error), expected_message)
  state <- readRDS(file.path(path, "state.rds"))
  receipt <- readRDS(file.path(path, "producer.rds"))
  attempts <- list.dirs(path, recursive = FALSE, full.names = TRUE)
  expect_length(attempts, 1L)
  expect_identical(receipt, readRDS(file.path(attempts, "receipt.rds")))
  expect_identical(state$status, "FAILED")
  expect_identical(receipt$status, "FAILED")
  expect_identical(state$error, conditionMessage(error))
  expect_identical(receipt$error, conditionMessage(error))
  expect_null(receipt$output)
  expect_false(file.exists(file.path(attempts, "result.rds")))
  expect_false(file.exists(file.path(path, "consumer.rds")))
  expect_false(file.exists(file.path(path, "READY")))
  expect_false(dir.exists(file.path(path, ".lock")))
}

test_that("an external handler aborts ordinary and directly signalled warnings", {
  root <- withr::local_tempdir()
  withr::local_seed(71)
  seed_before <- .Random.seed
  kind_before <- RNGkind()
  for (direct in c(FALSE, TRUE)) {
    for (warn in c(0L, 1L, 2L)) {
      withr::local_options(warn = warn)
      path <- file.path(root, paste(direct, warn, sep = "-"))
      seen <- 0L
      warning_call <- NULL
      caught <- tryCatch(
        withCallingHandlers(
          reflow_imaging_run(restart_plan(direct), path),
          warning = function(w) {
            seen <<- seen + 1L
            warning_call <<- conditionCall(w)
            invokeRestart("reflow_abort_stage", w)
          }
        ),
        error = identity
      )
      expect_identical(seen, 1L)
      expect_stage_aborted(path, caught)
      expect_identical(conditionCall(caught), warning_call)
      expect_identical(.Random.seed, seed_before)
      expect_identical(RNGkind(), kind_before)
      expect_identical(getOption("warn"), warn)
      expect_null(findRestart("reflow_abort_stage"))
    }
  }
})

test_that("aborting a seeded call restores an initially absent RNG state", {
  root <- withr::local_tempdir()
  withr::local_preserve_seed()
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  kind_before <- RNGkind()
  for (direct in c(FALSE, TRUE)) {
    path <- file.path(root, as.character(direct))
    caught <- tryCatch(
      withCallingHandlers(reflow_imaging_run(restart_plan(direct), path),
        warning = function(w) invokeRestart("reflow_abort_stage", w)),
      error = identity
    )
    expect_stage_aborted(path, caught)
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    expect_identical(RNGkind(), kind_before)
  }
})

test_that("a rejected stage can resume with its failed attempt preserved", {
  path <- file.path(withr::local_tempdir(), "retry")
  withr::local_options(warn = 2L)
  plan <- restart_plan()
  caught <- tryCatch(
    withCallingHandlers(reflow_imaging_run(plan, path), warning = function(w) {
      invokeRestart("reflow_abort_stage", w)
    }),
    error = identity
  )
  expect_stage_aborted(path, caught)
  old_attempt <- list.dirs(path, recursive = FALSE)
  old_receipt <- readRDS(file.path(old_attempt, "receipt.rds"))
  result <- withCallingHandlers(reflow_imaging_resume(plan, path), warning = function(w) {
    invokeRestart("muffleWarning")
  })
  expect_identical(result, list(producer = "stage protocol warning", consumer = "stage protocol warning"))
  expect_identical(readRDS(file.path(path, "state.rds"))$status, "READY")
  expect_identical(readRDS(file.path(path, "producer.rds"))$status, "READY")
  expect_identical(readRDS(file.path(path, "consumer.rds"))$status, "READY")
  expect_true(file.exists(file.path(path, "READY")))
  expect_length(list.dirs(path, recursive = FALSE), 3L)
  expect_identical(readRDS(file.path(old_attempt, "receipt.rds")), old_receipt)
  expect_false(file.exists(file.path(old_attempt, "result.rds")))
})

test_that("unused restart leaves direct signalling and missing RNG state unchanged", {
  path <- file.path(withr::local_tempdir(), "returning-handler")
  withr::local_options(warn = 2L)
  withr::local_preserve_seed()
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  kind_before <- RNGkind()
  seen <- 0L
  result <- withCallingHandlers(reflow_imaging_run(restart_plan(TRUE), path),
    warning = function(w) { seen <<- seen + 1L; invisible(NULL) })
  expect_identical(seen, 1L)
  expect_identical(result, list(producer = NULL, consumer = NULL))
  expect_true(file.exists(file.path(path, "READY")))
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  expect_identical(RNGkind(), kind_before)
  expect_null(findRestart("reflow_abort_stage"))
})

test_that("unused restart preserves native warning conversion to durable failure", {
  path <- file.path(withr::local_tempdir(), "native-warning")
  withr::local_options(warn = 2L)
  withr::local_seed(71)
  before <- .Random.seed
  caught <- tryCatch(reflow_imaging_run(restart_plan(), path), error = identity)
  expect_match(conditionMessage(caught), "stage protocol warning", fixed = TRUE)
  expect_stage_aborted(path, caught, expected_message = conditionMessage(caught))
  expect_identical(.Random.seed, before)
  expect_identical(getOption("warn"), 2L)
})
