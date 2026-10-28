v5_families <- c("spectral", "spatial", "omics", "clinical", "methods",
                 "simulation", "evidence", "platform", "brand")

setup_code <- function(scheme) {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(root, "code"))
  ret <- generate_setup_script(root, scheme = scheme)
  expect_true(file.exists(ret))
  expect_match(basename(ret), "00_setup.R", fixed = TRUE)
  paste(readLines(ret, warn = FALSE), collapse = "\n")
}

test_that("generate_setup_script writes 00_setup.R with scheme colors and palette", {
  for (s in v5_families) {
    scheme <- reflow_scheme(s)
    code <- setup_code(scheme)
    # accent and v5 neutrals embedded in wf_colors
    expect_match(code, paste0('primary       = "', scheme$primary, '"'), fixed = TRUE)
    expect_match(code, paste0('face          = "', scheme$face_light, '"'), fixed = TRUE)
    expect_match(code, paste0('border        = "', scheme$border_light, '"'), fixed = TRUE)
    expect_match(code, paste0('white         = "', scheme$page_light, '"'), fixed = TRUE)
    # full palette embedded as a c(...) vector
    for (col in scheme$palette) {
      expect_match(code, col, fixed = TRUE)
    }
    # key generated helpers present
    expect_match(code, "theme_wf <- function", fixed = TRUE)
    expect_match(code, "save_versioned <- function", fixed = TRUE)
    expect_match(code, "scale_color_wf_diverging", fixed = TRUE)
    # scheme label and family in the header
    expect_match(code, scheme$label, fixed = TRUE)
    expect_match(code, paste0("CTTIR v5 family: ", s), fixed = TRUE)
  }
})

test_that("generate_setup_script builds the palette as a quoted vector", {
  scheme <- reflow_scheme("brand")
  code <- setup_code(scheme)
  expected <- paste0('c("', paste(scheme$palette, collapse = '", "'), '")')
  expect_match(code, expected, fixed = TRUE)
})

test_that("generated fonts and colours come from the v5 scheme only", {
  scheme <- reflow_scheme("evidence")
  code <- setup_code(scheme)
  expect_match(code, 'body  = "Inter"', fixed = TRUE)
  expect_match(code, 'code  = "JetBrains Mono"', fixed = TRUE)
  expect_match(code, 'table.font.names         = c(wf_fonts$body, "system-ui", "sans-serif")',
               fixed = TRUE)
  expect_false(grepl("Source Sans", code, fixed = TRUE))
  expect_false(grepl('"(white|black)"', code))
  # Every literal hex colour in the script is a scheme value.
  hex <- unique(regmatches(code, gregexpr("#[0-9A-Fa-f]{6}", code))[[1]])
  expect_true(all(hex %in% unlist(scheme)))
})

test_that("text in generated tables and facets never sits on the accent", {
  code <- setup_code(reflow_scheme("methods"))
  expect_match(code, "column_labels.background.color = wf_colors$white", fixed = TRUE)
  expect_match(code, "column_labels.border.bottom.color = wf_colors$primary", fixed = TRUE)
  expect_match(code, "row_group.background.color     = wf_colors$face", fixed = TRUE)
  expect_match(code, "strip.background = ggplot2::element_rect(fill = wf_colors$face",
               fixed = TRUE)
  expect_false(grepl("background.color = wf_colors$primary", code, fixed = TRUE))
  # gt has no per-part font colour options; it derives them from the background.
  expect_false(grepl("column_labels.font.color|row_group.font.color", code))
})

test_that("the diverging scales use two distinct Okabe-Ito hues for every family", {
  for (s in v5_families) {
    scheme <- reflow_scheme(s)
    code <- setup_code(scheme)
    expect_match(code, "low = wf_colors$blue,", fixed = TRUE)
    expect_match(code, "high = wf_colors$red,", fixed = TRUE)
    expect_false(identical(scheme$blue, scheme$red))
  }
})

test_that("the setup script reports missing packages instead of installing them", {
  code <- parse(text = setup_code(reflow_scheme("brand")), keep.source = FALSE)
  used <- all.names(as.call(c(as.name("{"), as.list(code))))
  expect_false(any(c("install.packages", "p_load") %in% used))
  expect_false("pacman" %in% used)
  # Run the loader with one available and one missing package.
  first <- which(vapply(code, function(e) identical(e[[2]], as.name("wf_packages")),
                        logical(1)))
  env <- new.env()
  env$wf_packages <- c("stats", "reflowRabsentpackage")
  loader <- code[(first + 1):(first + 3)]
  expect_message(for (e in loader) eval(e, env), "reflowRabsentpackage")
  expect_identical(unname(env$wf_available), c(TRUE, FALSE))
})
