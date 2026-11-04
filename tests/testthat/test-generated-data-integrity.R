.generated_function <- function(code, name) {
  exprs <- parse(text = code)
  assignments <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
                          identical(e[[2]], as.name(name)), as.list(exprs))
  stopifnot(length(assignments) == 1L)
  env <- new.env(parent = globalenv())
  eval(assignments[[1]], env)
  env
}

test_that("versioned checkpoint collisions preserve the earlier object", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "code"))
  script <- generate_setup_script(root, reflow_scheme("brand"))
  env <- .generated_function(paste(readLines(script), collapse = "\n"), "save_versioned")
  env$Sys.time <- function() as.POSIXct("2026-01-02 03:04:05", tz = "UTC")
  first <- suppressMessages(env$save_versioned(list(value = 1), "step", root))
  before <- readBin(first, "raw", n = file.info(first)$size)
  expect_error(env$save_versioned(list(value = 2), "step", root), "already exists")
  expect_identical(readBin(first, "raw", n = file.info(first)$size), before)
  expect_identical(readRDS(first), list(value = 1))
  expect_match(basename(first), "^step_[0-9]{8}_[0-9]{6}[.]rds$")
})

test_that("failed checkpoint serialization publishes no partial result", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "code"))
  script <- generate_setup_script(root, reflow_scheme("brand"))
  env <- .generated_function(paste(readLines(script), collapse = "\n"), "save_versioned")
  env$saveRDS <- function(object, file, ...) {
    writeLines("partial", file)
    stop("injected serialization failure")
  }
  before <- list.files(root, all.files = TRUE)
  expect_error(env$save_versioned(1, "step", root), "injected serialization failure")
  expect_identical(list.files(root, all.files = TRUE), before)
})

test_that("generated numeric summaries retain missing rather than infinite estimates", {
  skip_if_not_installed("purrr")
  skip_if_not_installed("tibble")
  root <- withr::local_tempdir()
  dir.create(file.path(root, "code"))
  script <- generate_utils_script(root, reflow_scheme("brand"))
  env <- .generated_function(paste(readLines(script), collapse = "\n"), "numeric_summary")
  result <- expect_warning(env$numeric_summary(data.frame(empty = c(NA_real_, NA_real_),
                                    observed = c(2, 4))), NA)
  empty <- result[result$variable == "empty", ]
  expect_identical(empty$n, 0L)
  expect_true(all(is.na(empty[, c("mean", "sd", "median", "q25", "q75", "min", "max")])))
  observed <- result[result$variable == "observed", ]
  expect_equal(as.numeric(observed[, c("mean", "median", "q25", "q75", "min", "max")]),
               c(3, 3, 2.5, 3.5, 2, 4))
  expect_equal(observed$sd, sqrt(2))
  zero <- env$numeric_summary(data.frame(empty = numeric()))
  expect_identical(zero$n, 0L)
  expect_true(all(is.na(zero$mean)))
})

test_that("generated data dictionaries retain missing ranges and empty ratios", {
  skip_if_not_installed("purrr")
  skip_if_not_installed("tibble")
  env <- .generated_function(generate_data_io_content(reflow_scheme("brand")),
                              "wf_data_dict")
  result <- expect_warning(env$wf_data_dict(data.frame(empty = c(NA_real_, NA_real_),
                                                      observed = c(2, 4))), NA)
  expect_identical(result$min, c(NA_character_, "2"))
  expect_identical(result$max, c(NA_character_, "4"))
  expect_equal(result$pct_missing, c(100, 0))
  empty <- expect_warning(env$wf_data_dict(data.frame(value = numeric())), NA)
  expect_true(is.na(empty$pct_missing))
  expect_true(is.na(empty$min) && is.na(empty$max))
})
