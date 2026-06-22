test_that("reflow_preview writes HTML, invokes the viewer, and returns the scheme", {
  seen <- NULL
  html <- NULL
  res <- withr::with_options(
    list(viewer = function(url) {
      seen <<- url
      html <<- paste(readLines(url, warn = FALSE), collapse = "\n")
      invisible(url)
    }),
    reflow_preview("clinical")
  )
  # viewer was called with an html file
  expect_match(seen, "\\.html$")
  # returns scheme invisibly
  expect_invisible(withr::with_options(
    list(viewer = function(url) invisible(url)),
    reflow_preview("basic")
  ))
  expect_identical(res$name, "clinical")
  # generated HTML embeds scheme label, gradient and palette colors
  expect_match(html, reflow_scheme("clinical")$label, fixed = TRUE)
  expect_match(html, reflow_scheme("clinical")$gradient_from, fixed = TRUE)
  expect_match(html, "Palette (8 colors)", fixed = TRUE)
  expect_match(html, "Theme Colors", fixed = TRUE)
})

test_that("reflow_preview default scheme is basic", {
  html <- NULL
  withr::with_options(
    list(viewer = function(url) {
      html <<- paste(readLines(url, warn = FALSE), collapse = "\n")
      invisible(url)
    }),
    reflow_preview()
  )
  expect_match(html, reflow_scheme("basic")$label, fixed = TRUE)
})

test_that("reflow_preview errors for an invalid scheme name", {
  expect_error(
    withr::with_options(list(viewer = function(url) invisible(url)),
                        reflow_preview("neon"))
  )
})

test_that("reflow_preview aborts when htmltools is unavailable", {
  testthat::local_mocked_bindings(
    requireNamespace = function(pkg, ...) if (identical(pkg, "htmltools")) FALSE else TRUE,
    .package = "base"
  )
  expect_error(reflow_preview("basic"), "htmltools")
})
