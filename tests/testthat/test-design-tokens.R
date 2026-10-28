shipped_token_file <- function() {
  path <- system.file("cttir", "design_tokens.csv", package = "reflowR")
  expect_true(nzchar(path))
  path
}

file_bytes <- function(path) readBin(path, "raw", n = file.info(path)$size)

test_that("the shipped token table has the v5 structure", {
  tokens <- utils::read.csv(shipped_token_file(), stringsAsFactors = FALSE)
  expect_identical(names(tokens), c("group", "token", "value", "source"))
  expect_false(anyDuplicated(paste(tokens$group, tokens$token)) > 0)
  for (mode in c("light", "dark")) {
    expect_setequal(tokens$token[tokens$group == mode],
                    c("page", "text", "heading", "link", "code_surface", "border"))
  }
  expect_identical(tokens$value[tokens$group == "topic" & tokens$token == "brand"], "#0072B2")
})

test_that("the shipped tokens are byte-identical to tools/branding", {
  source <- testthat::test_path("..", "..", "tools", "branding", "design_tokens.csv")
  skip_if_not(file.exists(source), "tools/branding is not available")
  expect_identical(file_bytes(shipped_token_file()), file_bytes(source))
})

test_that("the shipped tokens are byte-identical to hexmakR's v5 library", {
  # hexmakR is a separate CTTIR package that is not on CRAN.
  skip_on_cran()
  skip_if_not_installed("hexmakR", minimum_version = "0.3.0")
  hexmakr <- system.file("cttir", "design_tokens.csv", package = "hexmakR")
  skip_if_not(nzchar(hexmakr), "hexmakR does not ship design_tokens.csv")
  expect_identical(file_bytes(shipped_token_file()), file_bytes(hexmakr))
})
