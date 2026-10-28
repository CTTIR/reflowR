v5_families <- c("spectral", "spatial", "omics", "clinical", "methods",
                 "simulation", "evidence", "platform", "brand")

icon_text <- function(family) {
  path <- system.file("icons", paste0("icon_", family, ".svg"), package = "reflowR")
  expect_true(nzchar(path))
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

test_that("every v5 family ships a small, well-formed hex icon", {
  for (s in v5_families) {
    svg <- icon_text(s)
    scheme <- reflow_scheme(s)
    expect_lt(nchar(svg, type = "bytes"), 2048L)
    expect_match(svg, '^<svg xmlns="http://www.w3.org/2000/svg" ')
    expect_match(svg, 'viewBox="0 0 56 64"', fixed = TRUE)
    expect_match(svg, "</svg>$")
    expect_identical(lengths(gregexpr("<g[ >]", svg)), lengths(gregexpr("</g>", svg, fixed = TRUE)))
    expect_false(grepl("&(?!amp;|lt;|gt;|quot;|#)", svg, perl = TRUE))
    # Pointy-top frame: tinted face, accent rim, transparent exterior.
    expect_match(svg, paste0('fill="', scheme$face_light, '" stroke="', scheme$accent, '"'),
                 fixed = TRUE)
    expect_false(grepl("<rect", svg, fixed = TRUE))
  }
})

test_that("only the v5 family icons are shipped", {
  icons <- list.files(system.file("icons", package = "reflowR"), pattern = "[.]svg$")
  expect_setequal(icons, paste0("icon_", v5_families, ".svg"))
})

test_that("reflow_icon_base64 returns a data URI for every family and alias", {
  for (s in c(v5_families, "basic", "code", "special", "other")) {
    uri <- suppressMessages(reflow_icon_base64(s))
    expect_type(uri, "character")
    expect_match(uri, "^data:image/svg\\+xml;base64,")
    expect_gt(nchar(uri), nchar("data:image/svg+xml;base64,"))
  }
  expect_identical(suppressMessages(reflow_icon_base64("basic")), reflow_icon_base64("brand"))
  expect_identical(suppressMessages(reflow_icon_base64("other")), reflow_icon_base64("platform"))
})

test_that("reflow_icon_base64 decodes back to the original SVG text", {
  uri <- reflow_icon_base64("clinical")
  b64 <- sub("^data:image/svg\\+xml;base64,", "", uri)
  decoded <- rawToChar(base64enc::base64decode(b64))
  expect_identical(decoded, icon_text("clinical"))
})

test_that("reflow_icon_base64 errors for a scheme with no icon file", {
  expect_error(reflow_icon_base64("nonexistent_scheme"), "Icon not found")
})
