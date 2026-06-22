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

test_that("reflow_init derives project name from directory basename when name is NULL", {
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "derived_name_proj")
  reflow_init(directory = proj, name = NULL, author = "A",
              scheme = "basic", depth = "minimal",
              git = FALSE, change_wd = FALSE, open = FALSE)
  # the derived name appears in the generated index.Rmd title
  idx <- readLines(file.path(proj, "analysis", "index.Rmd"), warn = FALSE)
  expect_true(any(grepl("derived_name_proj", idx, fixed = TRUE)))
  # and the .Rproj is named after the basename
  expect_true(file.exists(file.path(proj, "derived_name_proj.Rproj")))
})

test_that("reflow_init passes github_url and website into the navbar", {
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "nav_proj")
  reflow_init(directory = proj, author = "A", scheme = "code", depth = "minimal",
              github_url = "https://github.com/foo/bar",
              website = "https://example.org",
              git = FALSE, change_wd = FALSE, open = FALSE)
  yml <- readLines(file.path(proj, "analysis", "_site.yml"), warn = FALSE)
  expect_true(any(grepl("https://github.com/foo/bar", yml, fixed = TRUE)))
  expect_true(any(grepl("https://example.org", yml, fixed = TRUE)))
})

test_that("reflow_init with git = TRUE creates a repository and initial commit", {
  skip_if_not_installed("git2r")
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "git_proj")
  reflow_init(directory = proj, author = "A", scheme = "basic", depth = "minimal",
              git = TRUE, change_wd = FALSE, open = FALSE,
              user.name = "Test User", user.email = "test@example.org")
  expect_true(dir.exists(file.path(proj, ".git")))
  repo <- git2r::repository(proj)
  commits <- git2r::commits(repo)
  expect_gte(length(commits), 1L)
  expect_match(commits[[1]]$message, "reflowR")
})

test_that("reflow_init writes _workflowr.yml with the requested seed", {
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "seed_proj")
  reflow_init(directory = proj, author = "A", scheme = "basic", depth = "minimal",
              seed = 12345L, git = FALSE, change_wd = FALSE, open = FALSE)
  wf <- readLines(file.path(proj, "_workflowr.yml"), warn = FALSE)
  expect_true(any(grepl("seed: 12345", wf, fixed = TRUE)))
})

test_that("reflow_init errors on invalid depth", {
  tmp <- withr::local_tempdir()
  expect_error(
    reflow_init(directory = file.path(tmp, "baddepth"), author = "A",
                depth = "ultra", git = FALSE, change_wd = FALSE, open = FALSE)
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
