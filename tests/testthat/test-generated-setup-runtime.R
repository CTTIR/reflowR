run_generated_setup <- function(with_tables = FALSE) {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(root, "code"))
  path <- generate_setup_script(root, reflow_scheme("clinical"))
  env <- new.env(parent = globalenv())
  # Optional packages are deliberately absent except the table package under test.
  # Explicit namespaces in the generated script still execute the real packages.
  env$requireNamespace <- function(package, ...) {
    with_tables && identical(package, "gtsummary")
  }
  env$library <- function(...) invisible(NULL)
  suppressMessages(capture.output(sys.source(path, envir = env)))
  env
}

test_that("generated setup runs without optional table and analysis packages", {
  skip_if_not_installed("here")
  skip_if_not_installed("knitr")
  old_theme <- ggplot2::theme_get()
  old_chunks <- knitr::opts_chunk$get()
  on.exit(ggplot2::theme_set(old_theme), add = TRUE)
  on.exit(knitr::opts_chunk$set(old_chunks), add = TRUE)
  env <- run_generated_setup()
  expect_type(env$theme_wf, "closure")
  expect_true(dir.exists(env$paths$root))
  expect_false(exists("wf_gtsummary_theme", env, inherits = FALSE))
})

test_that("generated table theme executes and preserves summary definitions", {
  skip_if_not_installed("gtsummary", minimum_version = "2.0.0")
  skip_if_not_installed("here")
  skip_if_not_installed("knitr")
  old_theme <- ggplot2::theme_get()
  old_chunks <- knitr::opts_chunk$get()
  old_tables <- gtsummary::get_gtsummary_theme()
  on.exit(ggplot2::theme_set(old_theme), add = TRUE)
  on.exit(knitr::opts_chunk$set(old_chunks), add = TRUE)
  on.exit({
    gtsummary::reset_gtsummary_theme()
    gtsummary::set_gtsummary_theme(old_tables)
  }, add = TRUE)
  env <- run_generated_setup(with_tables = TRUE)
  expect_true(exists("wf_gtsummary_theme", env, inherits = FALSE))
  data <- data.frame(value = 1:12, group = rep(c("A", "B"), 6))
  table <- gtsummary::tbl_summary(data, digits = list(value ~ 2))
  values <- table$table_body$stat_0
  # gtsummary >= 2 uses quantile(type = 2): the quartiles of 1:12 are 3.5, 9.5.
  expect_true("6.50 (3.50, 9.50)" %in% values)
  expect_equal(sum(values == "6 (50%)", na.rm = TRUE), 2)
})
