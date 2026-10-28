test_that("reflow_init creates workflowr-compatible project", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "test_project")
  reflow_init(
    directory = proj, author = "Test Author",
    scheme = "brand", depth = "standard",
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
  reflow_init(directory = proj, author = "A", scheme = "platform",
              depth = "minimal", git = FALSE, change_wd = FALSE, open = FALSE)
  rmds <- list.files(file.path(proj, "analysis"), pattern = "^\\d+.*\\.Rmd$")
  expect_equal(length(rmds), 3)
})

test_that("standard preset creates 8 analysis files", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "std_test")
  reflow_init(directory = proj, author = "A", scheme = "brand",
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

test_that("all v5 families produce CSS with their accent and family icon", {
  skip_on_cran()
  for (s in c("spectral", "spatial", "omics", "clinical", "methods",
              "simulation", "evidence", "platform", "brand")) {
    tmp <- withr::local_tempdir()
    proj <- file.path(tmp, paste0("test_", s))
    reflow_init(directory = proj, author = "A", scheme = s,
                depth = "minimal", git = FALSE, change_wd = FALSE, open = FALSE)
    css <- readLines(file.path(proj, "assets", "theme.css"))
    expect_true(any(grepl(reflow_scheme(s)$accent, css, fixed = TRUE)))
    icon <- system.file("icons", paste0("icon_", s, ".svg"), package = "reflowR")
    for (copy in c("assets/icon.svg", "docs/assets/icon.svg")) {
      expect_identical(unname(tools::md5sum(file.path(proj, copy))), unname(tools::md5sum(icon)))
    }
  }
})

test_that("reflow_init defaults to the brand family", {
  expect_identical(eval(formals(reflow_init)$scheme)[1], "brand")
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "default_scheme")
  reflow_init(directory = proj, author = "A", depth = "minimal",
              git = FALSE, change_wd = FALSE, open = FALSE)
  css <- readLines(file.path(proj, "assets", "theme.css"))
  expect_true(any(grepl("#0072B2", css, fixed = TRUE)))
  readme <- readLines(file.path(proj, "README.md"))
  expect_true(any(grepl("Science, connected", readme, fixed = TRUE)))
})

test_that("earlier scheme names still create projects in their v5 family", {
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "alias_proj")
  suppressMessages(reflow_init(directory = proj, author = "A", scheme = "code",
                               depth = "minimal", git = FALSE, change_wd = FALSE,
                               open = FALSE))
  css <- readLines(file.path(proj, "assets", "theme.css"))
  expect_true(any(grepl("#56B4E9", css, fixed = TRUE)))
  icon <- system.file("icons", "icon_methods.svg", package = "reflowR")
  expect_identical(unname(tools::md5sum(file.path(proj, "assets", "icon.svg"))),
                   unname(tools::md5sum(icon)))
})

test_that("the generated _site.yml is valid and renders the site into docs/", {
  skip_if_not_installed("yaml")
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "yaml_proj")
  reflow_init(directory = proj, author = "A", scheme = "evidence", depth = "standard",
              github_url = "https://github.com/foo/bar", website = "https://example.org",
              git = FALSE, change_wd = FALSE, open = FALSE)
  site <- yaml::read_yaml(file.path(proj, "analysis", "_site.yml"))
  expect_identical(site$output_dir, "../docs")
  expect_match(site$navbar$title, '<img src="assets/icon.svg"', fixed = TRUE)
  expect_true(file.exists(file.path(proj, "docs", "assets", "icon.svg")))
  expect_length(site$navbar$left[[2]]$menu, 8L)
  expect_identical(vapply(site$navbar$right, `[[`, "", "href"),
                   c("https://github.com/foo/bar", "https://example.org"))
  expect_identical(site$output$`workflowr::wflow_html`$css, "../assets/theme.css")
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
              scheme = "brand", depth = "minimal",
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
  reflow_init(directory = proj, author = "A", scheme = "methods", depth = "minimal",
              github_url = "https://github.com/foo/bar",
              website = "https://example.org",
              git = FALSE, change_wd = FALSE, open = FALSE)
  yml <- readLines(file.path(proj, "analysis", "_site.yml"), warn = FALSE)
  expect_true(any(grepl("https://github.com/foo/bar", yml, fixed = TRUE)))
  expect_true(any(grepl("https://example.org", yml, fixed = TRUE)))
})

test_that("reflow_init with git = TRUE creates a repository and initial commit", {
  skip_if_not_installed("git2r")
  configured <- git2r::config(global = TRUE)
  skip_if(is.null(configured$user.name) || is.null(configured$user.email),
          "A configured user identity is required for the temporary commit test")
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "git_proj")
  reflow_init(directory = proj, author = "A", scheme = "brand", depth = "minimal",
              git = TRUE, change_wd = FALSE, open = FALSE)
  expect_true(dir.exists(file.path(proj, ".git")))
  repo <- git2r::repository(proj)
  commits <- git2r::commits(repo)
  expect_gte(length(commits), 1L)
  expect_match(commits[[1]]$message, "reflowR")
})

test_that("reflow_init writes _workflowr.yml with the requested seed", {
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "seed_proj")
  reflow_init(directory = proj, author = "A", scheme = "brand", depth = "minimal",
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
