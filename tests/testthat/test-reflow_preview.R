capture_preview <- function(...) {
  html <- NULL
  res <- withr::with_options(
    list(viewer = function(url) {
      html <<- paste(readLines(url, warn = FALSE), collapse = "\n")
      invisible(url)
    }),
    reflow_preview(...)
  )
  list(html = html, scheme = res)
}

test_that("reflow_preview writes HTML, invokes the viewer, and returns the scheme", {
  seen <- NULL
  res <- withr::with_options(
    list(viewer = function(url) {
      seen <<- url
      invisible(url)
    }),
    reflow_preview("clinical")
  )
  # viewer was called with an html file
  expect_match(seen, "\\.html$")
  # returns scheme invisibly
  expect_invisible(withr::with_options(
    list(viewer = function(url) invisible(url)),
    reflow_preview("brand")
  ))
  expect_identical(res$name, "clinical")
})

test_that("reflow_preview shows the v5 navbar, surfaces and palette", {
  p <- capture_preview("clinical")
  s <- reflow_scheme("clinical")
  expect_match(p$html, s$label, fixed = TRUE)
  expect_match(p$html, paste0("box-shadow:inset 0 -3px 0 ", s$accent), fixed = TRUE)
  expect_match(p$html, reflow_icon_base64("clinical"), fixed = TRUE)
  expect_match(p$html, "Palette (Okabe-Ito, accent first)", fixed = TRUE)
  expect_match(p$html, "Light surfaces", fixed = TRUE)
  expect_match(p$html, "Dark surfaces", fixed = TRUE)
  for (colour in c(s$palette, s$face_light, s$face_dark, s$page_dark, s$link_light)) {
    expect_match(p$html, colour, fixed = TRUE)
  }
  expect_false(grepl("gradient", p$html, fixed = TRUE))
})

test_that("reflow_preview default scheme is brand and aliases still preview", {
  expect_match(capture_preview()$html, reflow_scheme("brand")$label, fixed = TRUE)
  alias <- suppressMessages(capture_preview("special"))
  expect_identical(alias$scheme$family, "evidence")
})

test_that("reflow_preview errors for an invalid scheme name", {
  expect_error(
    withr::with_options(list(viewer = function(url) invisible(url)),
                        reflow_preview("neon"))
  )
})

test_that("reflow_preview does not need htmltools", {
  available <- base::requireNamespace
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "htmltools")) FALSE else available(package, ...)
    },
    .package = "base"
  )
  expect_match(capture_preview("brand")$html, reflow_scheme("brand")$label, fixed = TRUE)
})
