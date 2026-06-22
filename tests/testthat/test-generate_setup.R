test_that("generate_setup_script writes 00_setup.R with scheme colors and palette", {
  for (s in c("clinical", "basic", "code", "special", "other")) {
    root <- withr::local_tempdir()
    dir.create(file.path(root, "code"))
    scheme <- reflow_scheme(s)
    ret <- generate_setup_script(root, scheme = scheme)
    expect_true(file.exists(ret))
    expect_match(basename(ret), "00_setup.R", fixed = TRUE)
    code <- paste(readLines(ret, warn = FALSE), collapse = "\n")
    # primary color embedded in wf_colors
    expect_match(code, scheme$primary, fixed = TRUE)
    # full palette embedded as a c(...) vector
    for (col in scheme$palette) {
      expect_match(code, col, fixed = TRUE)
    }
    # key generated helpers present
    expect_match(code, "theme_wf <- function", fixed = TRUE)
    expect_match(code, "save_versioned <- function", fixed = TRUE)
    expect_match(code, "scale_color_wf_diverging", fixed = TRUE)
    # scheme label in the header
    expect_match(code, scheme$label, fixed = TRUE)
  }
})

test_that("generate_setup_script builds the palette as a quoted vector", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "code"))
  scheme <- reflow_scheme("basic")
  generate_setup_script(root, scheme = scheme)
  code <- paste(readLines(file.path(root, "code", "00_setup.R"), warn = FALSE),
                collapse = "\n")
  expected <- paste0('c("', paste(scheme$palette, collapse = '", "'), '")')
  expect_match(code, expected, fixed = TRUE)
})
