test_that("reflow_init creates workflowr-compatible project", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "test_project")
  reflow_init(
    directory = proj, author = "Test Author",
    scheme = "basic", depth = "standard",
    git = FALSE, change_wd = FALSE, open = FALSE
  )
  # workflowr base files
  expect_true(file.exists(file.path(proj, "_workflowr.yml")))
  expect_true(file.exists(file.path(proj, ".Rprofile")))
  expect_true(file.exists(file.path(proj, "test_project.Rproj")))

  # reflowR additions
  expect_true(file.exists(file.path(proj, "assets", "theme.css")))
  expect_true(file.exists(file.path(proj, "assets", "_theme.scss")))
  expect_true(file.exists(file.path(proj, "assets", "icon.svg")))
  expect_true(file.exists(file.path(proj, "code", "00_setup.R")))
  expect_true(file.exists(file.path(proj, "code", "utils.R")))
  expect_true(file.exists(file.path(proj, "code", "formulary.R")))
  expect_true(file.exists(file.path(proj, "code", "data_io.R")))
  expect_true(file.exists(file.path(proj, "code", "table_helpers.R")))
  expect_true(file.exists(file.path(proj, "code", "plot_helpers.R")))
  expect_true(file.exists(file.path(proj, "report", "report.qmd")))

  # _site.yml uses workflowr::wflow_html
  site_yml <- readLines(file.path(proj, "analysis", "_site.yml"))
  expect_true(any(grepl("workflowr::wflow_html", site_yml)))
  expect_true(any(grepl("theme.css", site_yml)))
})

test_that("minimal preset creates 3 analysis files", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "min_test")
  reflow_init(directory = proj, author = "A", scheme = "other",
              depth = "minimal", git = FALSE, change_wd = FALSE, open = FALSE)
  rmds <- list.files(file.path(proj, "analysis"), pattern = "^\\d+.*\\.Rmd$")
  expect_equal(length(rmds), 3)
})

test_that("standard preset creates 8 analysis files", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "std_test")
  reflow_init(directory = proj, author = "A", scheme = "basic",
              depth = "standard", git = FALSE, change_wd = FALSE, open = FALSE)
  rmds <- list.files(file.path(proj, "analysis"), pattern = "^\\d+.*\\.Rmd$")
  expect_equal(length(rmds), 8)
})

test_that("extended preset creates 12 analysis files", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "ext_test")
  reflow_init(directory = proj, author = "A", scheme = "clinical",
              depth = "extended", git = FALSE, change_wd = FALSE, open = FALSE)
  rmds <- list.files(file.path(proj, "analysis"), pattern = "^\\d+.*\\.Rmd$")
  expect_equal(length(rmds), 12)
})

test_that("all schemes produce valid CSS with scheme colors", {
  skip_on_cran()
  for (s in c("clinical", "basic", "code", "special", "other")) {
    tmp <- withr::local_tempdir()
    proj <- file.path(tmp, paste0("test_", s))
    reflow_init(directory = proj, author = "A", scheme = s,
                depth = "minimal", git = FALSE, change_wd = FALSE, open = FALSE)
    css <- readLines(file.path(proj, "assets", "theme.css"))
    expect_true(any(grepl("--primary", css)))
    expect_true(any(grepl(reflow_scheme(s)$primary, css, fixed = TRUE)))
  }
})

test_that("reflow_init errors on invalid scheme", {
  tmp <- withr::local_tempdir()
  expect_error(
    reflow_init(directory = file.path(tmp, "bad"), author = "A",
                scheme = "neon", git = FALSE, change_wd = FALSE, open = FALSE)
  )
})

test_that("existing = TRUE works on pre-existing directory", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "existing_proj")
  fs::dir_create(proj)
  writeLines("hello", file.path(proj, "my_data.csv"))
  reflow_init(directory = proj, author = "A", existing = TRUE,
              git = FALSE, change_wd = FALSE, open = FALSE)
  expect_true(file.exists(file.path(proj, "my_data.csv")))
  expect_true(file.exists(file.path(proj, "_workflowr.yml")))
  expect_true(file.exists(file.path(proj, "assets", "theme.css")))
})
