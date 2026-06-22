make_analysis_dir <- function() {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(root, "analysis"))
  root
}

test_that("generate_site_yml builds menu entries for every preset file", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "analysis"))
  preset <- reflow_preset("standard")
  ret <- generate_site_yml(root, name = "MyProj", scheme = reflow_scheme("basic"),
                           preset = preset)
  expect_true(file.exists(ret))
  yml <- paste(readLines(ret, warn = FALSE), collapse = "\n")
  expect_match(yml, "workflowr::wflow_html", fixed = TRUE)
  expect_match(yml, "../assets/theme.css", fixed = TRUE)
  expect_match(yml, 'name: "MyProj"', fixed = TRUE)
  # one href per pipeline file
  for (f in preset$files) {
    expect_match(yml, paste0(f, ".html"), fixed = TRUE)
  }
})

test_that("generate_site_yml adds github and website right-nav items when supplied", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "analysis"))
  generate_site_yml(root, name = "P", scheme = reflow_scheme("code"),
                    preset = reflow_preset("minimal"),
                    github_url = "https://github.com/foo/bar",
                    website = "https://example.org")
  yml <- paste(readLines(file.path(root, "analysis", "_site.yml"), warn = FALSE),
               collapse = "\n")
  expect_match(yml, "right:", fixed = TRUE)
  expect_match(yml, "fa-github", fixed = TRUE)
  expect_match(yml, "https://github.com/foo/bar", fixed = TRUE)
  expect_match(yml, "fa-globe", fixed = TRUE)
  expect_match(yml, "https://example.org", fixed = TRUE)
})

test_that("generate_site_yml omits right-nav block when no links given", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "analysis"))
  generate_site_yml(root, name = "P", scheme = reflow_scheme("other"),
                    preset = reflow_preset("minimal"))
  yml <- paste(readLines(file.path(root, "analysis", "_site.yml"), warn = FALSE),
               collapse = "\n")
  expect_false(grepl("fa-github", yml, fixed = TRUE))
  expect_false(grepl("fa-globe", yml, fixed = TRUE))
})

test_that("generate_site_yml only github (no website) yields github but not globe", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "analysis"))
  generate_site_yml(root, name = "P", scheme = reflow_scheme("special"),
                    preset = reflow_preset("minimal"),
                    github_url = "https://github.com/x/y")
  yml <- paste(readLines(file.path(root, "analysis", "_site.yml"), warn = FALSE),
               collapse = "\n")
  expect_match(yml, "fa-github", fixed = TRUE)
  expect_false(grepl("fa-globe", yml, fixed = TRUE))
})

test_that("generate_site_yml capitalises and numbers menu step text", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "analysis"))
  generate_site_yml(root, name = "P", scheme = reflow_scheme("basic"),
                    preset = reflow_preset("minimal"))
  yml <- paste(readLines(file.path(root, "analysis", "_site.yml"), warn = FALSE),
               collapse = "\n")
  # "01_read_data" -> "01 -- Read data"
  expect_match(yml, "01 -- Read data", fixed = TRUE)
})
