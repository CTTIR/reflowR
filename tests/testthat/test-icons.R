test_that("reflow_icon_base64 returns a data URI for every shipped icon", {
  for (s in c("clinical", "basic", "code", "special", "other")) {
    uri <- reflow_icon_base64(s)
    expect_type(uri, "character")
    expect_match(uri, "^data:image/svg\\+xml;base64,")
    expect_gt(nchar(uri), nchar("data:image/svg+xml;base64,"))
  }
})

test_that("reflow_icon_base64 decodes back to the original SVG text", {
  uri <- reflow_icon_base64("clinical")
  b64 <- sub("^data:image/svg\\+xml;base64,", "", uri)
  decoded <- rawToChar(base64enc::base64decode(b64))
  icon_path <- system.file("icons", "icon_clinical.svg", package = "reflowR")
  original <- paste(readLines(icon_path, warn = FALSE), collapse = "\n")
  expect_identical(decoded, original)
})

test_that("reflow_icon_base64 errors for a scheme with no icon file", {
  expect_error(reflow_icon_base64("nonexistent_scheme"), "Icon not found")
})
