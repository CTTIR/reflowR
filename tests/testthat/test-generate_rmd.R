test_that("generate_rmd writes an analysis Rmd with title, author, description", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "analysis"))
  ret <- generate_rmd(root, file_name = "01_read_data", author = "A. Author",
                      description = "Import the data",
                      scheme = reflow_scheme("basic"),
                      preset = reflow_preset("standard"))
  expect_true(file.exists(ret))
  rmd <- paste(readLines(ret, warn = FALSE), collapse = "\n")
  expect_match(rmd, 'title: "01 -- Read data"', fixed = TRUE)
  expect_match(rmd, 'author: "A. Author"', fixed = TRUE)
  expect_match(rmd, "Import the data", fixed = TRUE)
  expect_match(rmd, 'source(here::here("code", "00_setup.R"))', fixed = TRUE)
  expect_match(rmd, "sessionInfo()", fixed = TRUE)
})

test_that("generate_rmd capitalises multi-word step titles", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "analysis"))
  generate_rmd(root, file_name = "04_quality_control", author = "A",
               description = "QC step", scheme = reflow_scheme("code"),
               preset = reflow_preset("extended"))
  rmd <- paste(readLines(file.path(root, "analysis", "04_quality_control.Rmd"),
                         warn = FALSE), collapse = "\n")
  expect_match(rmd, 'title: "04 -- Quality control"', fixed = TRUE)
})

test_that("generate_rmd fallback content is used for unknown steps", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "analysis"))
  generate_rmd(root, file_name = "99_mystery", author = "A",
               description = "Unknown", scheme = reflow_scheme("basic"),
               preset = reflow_preset("minimal"))
  rmd <- paste(readLines(file.path(root, "analysis", "99_mystery.Rmd"),
                         warn = FALSE), collapse = "\n")
  expect_match(rmd, "## TODO", fixed = TRUE)
})

test_that("get_rmd_content dispatches to a distinct body for every known step", {
  steps <- c("read_data", "tidy", "eda", "preprocess", "quality_control",
             "descriptive", "inference", "modelling", "model_diagnostics",
             "dim_reduction", "sensitivity", "sub_reports", "report",
             "final_report")
  bodies <- vapply(steps, function(st) {
    as.character(get_rmd_content(paste0("01_", st),
                                 reflow_scheme("basic"),
                                 reflow_preset("standard")))
  }, character(1))
  # each known step returns non-empty content
  expect_true(all(nzchar(bodies)))
  # no known step falls through to the TODO fallback
  expect_false(any(grepl("## TODO", bodies, fixed = TRUE)))
  # bodies are not all identical (dispatch genuinely differentiates)
  expect_gt(length(unique(bodies)), 10)
})

test_that("get_rmd_content returns the TODO fallback for unknown steps", {
  body <- as.character(get_rmd_content("07_does_not_exist",
                                       reflow_scheme("basic"),
                                       reflow_preset("standard")))
  expect_match(body, "## TODO", fixed = TRUE)
})

test_that("each get_content_* helper returns non-empty character content", {
  gens <- c("get_content_read_data", "get_content_tidy", "get_content_eda",
            "get_content_preprocess", "get_content_quality_control",
            "get_content_descriptive", "get_content_inference",
            "get_content_modelling", "get_content_model_diagnostics",
            "get_content_dim_reduction", "get_content_sensitivity",
            "get_content_sub_reports", "get_content_report_rmd",
            "get_content_final_report")
  for (g in gens) {
    fn <- get(g, envir = asNamespace("reflowR"))
    txt <- as.character(fn())
    expect_true(nzchar(txt), info = g)
  }
})

test_that("generate_index_rmd includes email contact line when email given", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "analysis"))
  ret <- generate_index_rmd(root, name = "MyProj", author = "A. Author",
                            email = "a@example.org",
                            scheme = reflow_scheme("basic"),
                            preset = reflow_preset("standard"))
  rmd <- paste(readLines(ret, warn = FALSE), collapse = "\n")
  expect_match(rmd, "[a@example.org](mailto:a@example.org)", fixed = TRUE)
  expect_match(rmd, "Analysis Pipeline", fixed = TRUE)
  # a table row per preset file
  for (f in reflow_preset("standard")$files) {
    expect_match(rmd, paste0("(", f, ".html)"), fixed = TRUE)
  }
})

test_that("generate_index_rmd omits contact line when email is NULL", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "analysis"))
  generate_index_rmd(root, name = "MyProj", author = "A. Author", email = NULL,
                     scheme = reflow_scheme("basic"),
                     preset = reflow_preset("minimal"))
  rmd <- paste(readLines(file.path(root, "analysis", "index.Rmd"), warn = FALSE),
               collapse = "\n")
  expect_false(grepl("mailto:", rmd, fixed = TRUE))
})

test_that("generate_about_rmd includes all author detail lines when supplied", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "analysis"))
  ret <- generate_about_rmd(root, name = "MyProj", author = "A. Author",
                            email = "a@example.org", affiliation = "Uni X",
                            orcid = "0000-0000-0000-0001",
                            website = "https://example.org",
                            scheme = reflow_scheme("clinical"),
                            preset = reflow_preset("standard"))
  rmd <- paste(readLines(ret, warn = FALSE), collapse = "\n")
  expect_match(rmd, "**Email:**", fixed = TRUE)
  expect_match(rmd, "**Affiliation:** Uni X", fixed = TRUE)
  expect_match(rmd, "https://orcid.org/0000-0000-0000-0001", fixed = TRUE)
  expect_match(rmd, "**Website:**", fixed = TRUE)
  expect_match(rmd, reflow_scheme("clinical")$primary, fixed = TRUE)
})

test_that("generate_about_rmd omits detail lines when all optional args NULL", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "analysis"))
  generate_about_rmd(root, name = "MyProj", author = "A. Author",
                     email = NULL, affiliation = NULL, orcid = NULL,
                     website = NULL, scheme = reflow_scheme("other"),
                     preset = reflow_preset("minimal"))
  rmd <- paste(readLines(file.path(root, "analysis", "about.Rmd"), warn = FALSE),
               collapse = "\n")
  expect_false(grepl("**Email:**", rmd, fixed = TRUE))
  expect_false(grepl("**ORCID:**", rmd, fixed = TRUE))
  expect_match(rmd, "Pipeline Documentation", fixed = TRUE)
})

test_that("rmd generators return invisibly", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "analysis"))
  expect_invisible(generate_rmd(root, "01_read_data", "A", "desc",
                                reflow_scheme("basic"), reflow_preset("minimal")))
  expect_invisible(generate_index_rmd(root, "P", "A", NULL,
                                      reflow_scheme("basic"), reflow_preset("minimal")))
  expect_invisible(generate_about_rmd(root, "P", "A", NULL, NULL, NULL, NULL,
                                      reflow_scheme("basic"), reflow_preset("minimal")))
})
