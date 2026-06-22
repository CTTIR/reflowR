test_that("generate_css writes a CSS file with scheme colors and custom properties", {
  for (s in c("clinical", "basic", "code", "special", "other")) {
    out <- withr::local_tempfile(fileext = ".css")
    scheme <- reflow_scheme(s)
    ret <- generate_css(scheme, out)
    expect_identical(ret, out)
    expect_true(file.exists(out))
    css <- paste(readLines(out, warn = FALSE), collapse = "\n")
    # custom properties present
    expect_match(css, "--primary:", fixed = TRUE)
    expect_match(css, "--gradient-from:", fixed = TRUE)
    # the scheme's actual colors are interpolated
    expect_match(css, scheme$primary, fixed = TRUE)
    expect_match(css, scheme$light_grey, fixed = TRUE)
    # label appears in the header comment
    expect_match(css, scheme$label, fixed = TRUE)
    # print media query branch exists
    expect_match(css, "@media print", fixed = TRUE)
  }
})

test_that("generate_css return value is invisible", {
  out <- withr::local_tempfile(fileext = ".css")
  expect_invisible(generate_css(reflow_scheme("basic"), out))
})

test_that("generate_scss writes Quarto SCSS with defaults and rules sections", {
  for (s in c("clinical", "basic", "code", "special", "other")) {
    out <- withr::local_tempfile(fileext = ".scss")
    scheme <- reflow_scheme(s)
    ret <- generate_scss(scheme, out)
    expect_identical(ret, out)
    scss <- paste(readLines(out, warn = FALSE), collapse = "\n")
    expect_match(scss, "scss:defaults", fixed = TRUE)
    expect_match(scss, "scss:rules", fixed = TRUE)
    expect_match(scss, paste0("$primary: ", scheme$primary), fixed = TRUE)
    expect_match(scss, "$navbar-bg: $primary", fixed = TRUE)
  }
})

test_that("generate_scss return value is invisible", {
  out <- withr::local_tempfile(fileext = ".scss")
  expect_invisible(generate_scss(reflow_scheme("clinical"), out))
})
