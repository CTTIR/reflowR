v5_families <- c("spectral", "spatial", "omics", "clinical", "methods",
                 "simulation", "evidence", "platform", "brand")

read_text <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

# Declarations whose property is exactly `color` (text), or a background.
text_colour_values <- function(css) {
  m <- regmatches(css, gregexpr("(^|[\\s;{])color:\\s*[^;]+;", css, perl = TRUE))[[1]]
  sub("^.*color:\\s*", "", m)
}
background_values <- function(css) {
  m <- regmatches(css, gregexpr("background(-color)?:\\s*[^;]+;", css, perl = TRUE))[[1]]
  sub("^background(-color)?:\\s*", "", m)
}

test_that("generate_css writes the v5 surface with light and dark tokens", {
  for (s in v5_families) {
    out <- withr::local_tempfile(fileext = ".css")
    scheme <- reflow_scheme(s)
    ret <- generate_css(scheme, out)
    expect_identical(ret, out)
    css <- read_text(out)
    expect_match(css, scheme$label, fixed = TRUE)
    expect_match(css, paste0("--cttir-topic: ", scheme$accent, ";"), fixed = TRUE)
    for (mode in c("light", "dark")) {
      for (field in c("page", "text", "heading", "link", "code_surface", "border")) {
        property <- paste0("--cttir-", gsub("_", "-", field), ": ")
        expect_match(css, paste0(property, scheme[[paste0(field, "_", mode)]], ";"), fixed = TRUE)
      }
      expect_match(css, paste0("--cttir-face: ", scheme[[paste0("face_", mode)]], ";"), fixed = TRUE)
    }
    expect_match(css, "@media (prefers-color-scheme: dark) {\n:root {", fixed = TRUE)
    expect_match(css, '[data-bs-theme="dark"], body.colorscheme-dark {', fixed = TRUE)
    expect_match(css, '[data-bs-theme="light"], body.colorscheme-light {', fixed = TRUE)
    expect_match(css, paste0('[data-cttir-topic="', s, '"] { --cttir-topic: ', scheme$accent, "; }"),
                 fixed = TRUE)
    expect_match(css, "@media print", fixed = TRUE)
  }
})

test_that("generate_css uses system font stacks and never downloads fonts", {
  out <- withr::local_tempfile(fileext = ".css")
  generate_css(reflow_scheme("brand"), out)
  css <- read_text(out)
  expect_match(css, '--cttir-font-body: "Inter", system-ui, sans-serif;', fixed = TRUE)
  expect_match(css, '--cttir-font-code: "JetBrains Mono", ui-monospace, monospace;', fixed = TRUE)
  expect_match(css, "--cttir-brand-weight: 600;", fixed = TRUE)
  expect_false(grepl("@import", css, fixed = TRUE))
  expect_false(grepl("@font-face", css, fixed = TRUE))
  expect_false(grepl("https?://", css))
  expect_false(grepl("Source Sans", css, fixed = TRUE))
})

test_that("the navbar is flat on the page background with an accent rule", {
  out <- withr::local_tempfile(fileext = ".css")
  generate_css(reflow_scheme("clinical"), out)
  css <- read_text(out)
  expect_false(grepl("gradient", css, fixed = TRUE))
  expect_match(css, ".navbar, .navbar-default {\n  background: var(--cttir-page);", fixed = TRUE)
  expect_match(css, "box-shadow: inset 0 -3px 0 var(--cttir-topic);", fixed = TRUE)
  expect_match(css, "font-family: var(--cttir-font-brand);\n  font-weight: var(--cttir-brand-weight);",
               fixed = TRUE)
  for (selector in c(".navbar-default .navbar-nav > li > a", ".dropdown-menu", ".tocify",
                     ".table-striped", ".panel-default > .panel-heading", "footer",
                     ".btn-default.btn-xs", "body pre:not([class])")) {
    expect_match(css, selector, fixed = TRUE)
  }
})

test_that("links use the link token and the accent never colours text or its background", {
  for (s in v5_families) {
    out <- withr::local_tempfile(fileext = ".css")
    scheme <- reflow_scheme(s)
    generate_css(scheme, out)
    css <- read_text(out)
    expect_match(css, "a, a:visited {\n  color: var(--cttir-link);", fixed = TRUE)
    text <- text_colour_values(css)
    expect_true(length(text) > 20)
    expect_false(any(grepl("--cttir-topic|--cttir-face", text)))
    expect_false(any(grepl(scheme$accent, text, fixed = TRUE)))
    backgrounds <- background_values(css)
    expect_false(any(grepl("--cttir-topic", backgrounds, fixed = TRUE)))
    expect_false(any(grepl(scheme$accent, backgrounds, fixed = TRUE)))
    # The h2 rule is neutral; h1 carries the accent underline.
    expect_match(css, "h2 {\n  border-bottom: 1px solid var(--cttir-border);", fixed = TRUE)
    expect_match(css, "h1, h1.title {\n  border-bottom: 3px solid var(--cttir-topic);", fixed = TRUE)
  }
})

test_that("code tokens override the pandoc highlight style in both modes", {
  scheme <- reflow_scheme("methods")
  out <- withr::local_tempfile(fileext = ".css")
  generate_css(scheme, out)
  css <- read_text(out)
  expect_match(css, "body code span.kw, ", fixed = TRUE)
  expect_match(css, "{ color: var(--cttir-syntax-keyword); }", fixed = TRUE)
  expect_match(css, "{ color: var(--cttir-syntax-comment); font-style: italic; }", fixed = TRUE)
  for (mode in c("light", "dark")) {
    syntax <- scheme[[paste0("syntax_", mode)]]
    expect_match(css, paste0("--cttir-syntax-keyword: ", syntax[["keyword"]], ";"), fixed = TRUE)
  }
})

test_that("earlier scheme names produce the stylesheet of their v5 family", {
  a <- withr::local_tempfile(fileext = ".css")
  b <- withr::local_tempfile(fileext = ".css")
  suppressMessages(generate_css(reflow_scheme("code"), a))
  generate_css(reflow_scheme("methods"), b)
  expect_identical(read_text(a), read_text(b))
})

test_that("generate_css return value is invisible", {
  out <- withr::local_tempfile(fileext = ".css")
  expect_invisible(generate_css(reflow_scheme("brand"), out))
})

test_that("generate_scss writes Quarto SCSS on the v5 tokens", {
  for (s in v5_families) {
    out <- withr::local_tempfile(fileext = ".scss")
    scheme <- reflow_scheme(s)
    ret <- generate_scss(scheme, out)
    expect_identical(ret, out)
    scss <- read_text(out)
    expect_match(scss, "scss:defaults", fixed = TRUE)
    expect_match(scss, "scss:rules", fixed = TRUE)
    expect_lt(regexpr("scss:defaults", scss, fixed = TRUE), regexpr("scss:rules", scss, fixed = TRUE))
    expect_match(scss, paste0("$cttir-topic: ", scheme$accent, ";"), fixed = TRUE)
    expect_match(scss, paste0("$cttir-page: ", scheme$page_light, ";"), fixed = TRUE)
    expect_match(scss, "$primary: $cttir-link;", fixed = TRUE)
    expect_match(scss, "$link-color: $cttir-link;", fixed = TRUE)
    expect_match(scss, "$navbar-bg: $cttir-page;", fixed = TRUE)
    expect_match(scss, '$font-family-sans-serif: "Inter", system-ui, sans-serif;', fixed = TRUE)
    expect_match(scss, "@media (prefers-color-scheme: dark)", fixed = TRUE)
    expect_match(scss, paste0("--cttir-page: ", scheme$page_dark, ";"), fixed = TRUE)
    expect_false(grepl("gradient|@import|https?://", scss))
    expect_false(any(grepl("--cttir-topic", text_colour_values(scss), fixed = TRUE)))
  }
})

test_that("generate_scss return value is invisible", {
  out <- withr::local_tempfile(fileext = ".scss")
  expect_invisible(generate_scss(reflow_scheme("clinical"), out))
})
