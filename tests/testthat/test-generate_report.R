test_that("generate_report_qmd writes report.qmd with dual html/pdf format", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "report"))
  ret <- generate_report_qmd(root, name = "MyProj", author = "A. Author",
                             scheme = reflow_scheme("basic"),
                             preset = reflow_preset("standard"))
  expect_true(file.exists(ret))
  qmd <- paste(readLines(ret, warn = FALSE), collapse = "\n")
  expect_match(qmd, 'title: "MyProj -- Report"', fixed = TRUE)
  expect_match(qmd, 'author: "A. Author"', fixed = TRUE)
  expect_match(qmd, "../assets/_theme.scss", fixed = TRUE)
  expect_match(qmd, "format:", fixed = TRUE)
  expect_match(qmd, "pdf:", fixed = TRUE)
  expect_match(qmd, "sessionInfo()", fixed = TRUE)
})

test_that("generate_project_readme writes README with project metadata", {
  root <- withr::local_tempdir()
  ret <- generate_project_readme(root, name = "MyProj", author = "A. Author",
                                 scheme = reflow_scheme("clinical"),
                                 preset = reflow_preset("extended"))
  expect_true(file.exists(ret))
  md <- paste(readLines(ret, warn = FALSE), collapse = "\n")
  expect_match(md, "# MyProj", fixed = TRUE)
  expect_match(md, "**Author:** A. Author", fixed = TRUE)
  expect_match(md, reflow_scheme("clinical")$label, fixed = TRUE)
  expect_match(md, reflow_preset("extended")$label, fixed = TRUE)
  expect_match(md, "Project Structure", fixed = TRUE)
})

test_that("report and readme generators return invisibly", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "report"))
  expect_invisible(generate_report_qmd(root, name = "P", author = "A",
                                       scheme = reflow_scheme("code"),
                                       preset = reflow_preset("minimal")))
  expect_invisible(generate_project_readme(root, name = "P", author = "A",
                                           scheme = reflow_scheme("code"),
                                           preset = reflow_preset("minimal")))
})
