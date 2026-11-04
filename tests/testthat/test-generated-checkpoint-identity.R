checkpoint_helper <- function(code, name) {
  env <- new.env(parent = globalenv())
  expressions <- parse(text = code, keep.source = FALSE)
  for (expr in expressions) {
    if (is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
        identical(expr[[2L]], as.name(name))) eval(expr, env)
  }
  env[[name]]
}

test_that("checkpoint step prefixes are literal and timestamp suffixes bounded", {
  load_checkpoint <- checkpoint_helper(generate_formulary_content(reflow_scheme("brand")),
                                       "wf_load_checkpoint")
  root <- withr::local_tempdir()
  origin <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  names <- c("a.b", "dose+followup", "sample[1]", "Müller")
  for (step in names) {
    older <- file.path(root, paste0(step, "_20260101_000000.rds"))
    newer <- file.path(root, paste0(step, "_20260101_000001.rds"))
    saveRDS(list(step = step, value = 1), older)
    saveRDS(list(step = step, value = 2), newer)
    Sys.setFileTime(older, origin)
    Sys.setFileTime(newer, origin + 1)
    # These superficially similar files must not take precedence.
    for (suffix in c("_20260101_000001.rds.bak", "_20260101_000001Xrds",
                     "_20260101_00001.rds", "_20260101_000001_extra.rds")) {
      invalid <- file.path(root, paste0(step, suffix))
      saveRDS(list(step = "wrong", value = 999), invalid)
      Sys.setFileTime(invalid, origin + 20)
    }
    lookalike <- file.path(root, "aXb_20260101_000002.rds")
    saveRDS(list(step = "aXb", value = 999), lookalike)
    Sys.setFileTime(lookalike, origin + 30)
    expect_identical(load_checkpoint(step, root), list(step = step, value = 2))
  }
  # Modification time, rather than the filename timestamp, remains decisive.
  Sys.setFileTime(older, origin + 2)
  expect_identical(load_checkpoint(step, root), list(step = step, value = 1))
  for (step in list(NULL, character(), c("a", "b"), NA_character_, "", 1)) {
    expect_error(load_checkpoint(step, root), "step_name")
  }
  expect_error(load_checkpoint("not present", root), "No checkpoint")
})

test_that("the generated checkpoint writer remains readable with exact names", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "code"))
  script <- generate_setup_script(root, reflow_scheme("brand"))
  save_checkpoint <- checkpoint_helper(paste(readLines(script), collapse = "\n"),
                                      "save_versioned")
  load_checkpoint <- checkpoint_helper(generate_formulary_content(reflow_scheme("brand")),
                                       "wf_load_checkpoint")
  expected <- list(step = "visit.1+[Müller]", values = c(-2, 0, 3))
  saved <- save_checkpoint(expected, expected$step, root)
  before <- readBin(saved, "raw", n = file.info(saved)$size)
  expect_identical(load_checkpoint(expected$step, root), expected)
  expect_identical(readBin(saved, "raw", n = file.info(saved)$size), before)
})
