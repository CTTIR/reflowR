# The generated table helpers are evaluated here, not only matched as text.
generated_table_helpers <- function() {
  env <- new.env(parent = globalenv())
  code <- generate_table_helpers_content(reflow_scheme("clinical"))
  for (expr in parse(text = code)) {
    if (is.call(expr) && identical(expr[[1L]], as.name("source"))) next
    eval(expr, envir = env)
  }
  env
}

test_that("generated inline intervals print estimate, lower and upper bound", {
  env <- generated_table_helpers()
  # The estimate used to be printed again in place of the lower bound.
  expect_identical(env$wf_inline(c(1.2, 0.8, 1.9), type = "ci"),
                   "1.20 (95% CI: 0.80, 1.90)")
  expect_identical(env$wf_inline(c(0.8, 1.9), type = "ci", digits = 1),
                   "95% CI: 0.8, 1.9")
  expect_identical(env$wf_inline(1.234, type = "ci"), "1.23")
  expect_identical(env$wf_inline(0.0004, type = "p"), "< 0.001")
  expect_identical(env$wf_inline(0.004, type = "p"), "0.004")
  expect_identical(env$wf_inline(3.14159, digits = 3, prefix = "b = ", suffix = " mm"),
                   "b = 3.142 mm")
})

test_that("generated summary table runs with its default column selection", {
  skip_if_not_installed("gtsummary", minimum_version = "2.0.0")
  skip_if_not_installed("dplyr")
  env <- generated_table_helpers()
  d <- data.frame(age = c(31, 45, 52, 38, 60, 47), sex = rep(c("f", "m"), 3),
                  arm = rep(c("a", "b"), each = 3))
  # The default selection helper, everything(), was evaluated outside a selection.
  all_columns <- env$wf_tbl_summary(d)
  expect_s3_class(all_columns, "tbl_summary")
  expect_setequal(unique(all_columns$table_body$variable), c("age", "sex", "arm"))
  chosen <- env$wf_tbl_summary(d, include = c("age", "sex"))
  expect_setequal(unique(chosen$table_body$variable), c("age", "sex"))
  # Bare column names are a selection too and must reach tbl_summary() unevaluated.
  bare <- env$wf_tbl_summary(d, include = c(age, arm))
  expect_setequal(unique(bare$table_body$variable), c("age", "arm"))
})
