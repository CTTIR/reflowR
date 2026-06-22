test_that("generate_code_library writes all four library files", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "code"))
  ret <- generate_code_library(root, scheme = reflow_scheme("basic"),
                               preset = reflow_preset("standard"))
  expect_length(ret, 4)
  for (f in c("formulary.R", "data_io.R", "table_helpers.R", "plot_helpers.R")) {
    expect_true(file.exists(file.path(root, "code", f)))
  }
})

test_that("formulary content contains analysis recipes", {
  txt <- as.character(generate_formulary_content(reflow_scheme("clinical")))
  expect_match(txt, "formulary.R", fixed = TRUE)
  expect_match(txt, "wf_normality_battery <- function", fixed = TRUE)
  expect_match(txt, "wf_compare_groups <- function", fixed = TRUE)
  expect_match(txt, "shapiro.test", fixed = TRUE)
  expect_match(txt, "wf_load_checkpoint <- function", fixed = TRUE)
})

test_that("data_io content contains read/write helpers", {
  txt <- as.character(generate_data_io_content(reflow_scheme("code")))
  expect_match(txt, "data_io.R", fixed = TRUE)
  expect_match(txt, "wf_read_data <- function", fixed = TRUE)
  expect_match(txt, "wf_read_all <- function", fixed = TRUE)
  expect_match(txt, "wf_data_dict <- function", fixed = TRUE)
})

test_that("table_helpers content references gt and gtsummary helpers", {
  txt <- as.character(generate_table_helpers_content(reflow_scheme("special")))
  expect_match(txt, "table_helpers.R", fixed = TRUE)
  expect_match(txt, "wf_tbl_summary <- function", fixed = TRUE)
  expect_match(txt, "wf_crosstab <- function", fixed = TRUE)
})

test_that("plot_helpers content references plotting helpers", {
  txt <- as.character(generate_plot_helpers_content(reflow_scheme("other")))
  expect_match(txt, "plot_helpers.R", fixed = TRUE)
  expect_match(txt, "wf_forest_plot <- function", fixed = TRUE)
  expect_match(txt, "wf_roc_plot <- function", fixed = TRUE)
})

test_that("content generators are stable across schemes (scheme-independent body)", {
  # The content templates take a scheme arg but emit scheme-independent code;
  # verify they return identical, non-empty content for any scheme.
  ref <- as.character(generate_formulary_content(reflow_scheme("basic")))
  expect_true(nzchar(ref))
  for (s in c("clinical", "code", "special", "other")) {
    expect_identical(as.character(generate_formulary_content(reflow_scheme(s))), ref)
  }
})

test_that("generate_code_library returns invisibly", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "code"))
  expect_invisible(generate_code_library(root, scheme = reflow_scheme("basic"),
                                         preset = reflow_preset("minimal")))
})
