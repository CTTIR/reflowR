test_that("reflow_scheme returns valid scheme for all names", {
  for (s in c("clinical", "basic", "code", "special", "other")) {
    scheme <- reflow_scheme(s)
    expect_true(is.list(scheme))
    expect_true(all(c("name", "primary", "accent", "palette") %in% names(scheme)))
    expect_equal(length(scheme$palette), 8)
    expect_true(grepl("^#[0-9A-Fa-f]{6}$", scheme$primary))
  }
})

test_that("reflow_scheme errors on unknown scheme", {
  expect_error(reflow_scheme("neon"))
})

test_that("reflow_schemes returns all scheme names", {
  result <- reflow_schemes()
  expect_equal(sort(result), sort(c("clinical", "basic", "code", "special", "other")))
})

test_that("reflow_schemes prints one line per scheme and returns invisibly", {
  out <- capture.output(res <- reflow_schemes(), type = "message")
  expect_invisible(reflow_schemes())
  # header plus one entry per scheme
  for (s in c("clinical", "basic", "code", "special", "other")) {
    expect_true(any(grepl(s, out)))
  }
  expect_length(res, 5)
})

test_that("reflow_scheme supports partial matching via match.arg", {
  expect_identical(reflow_scheme("clin")$name, "clinical")
})

test_that("scheme definitions are stable (snapshot of all schemes)", {
  all_schemes <- lapply(c("clinical", "basic", "code", "special", "other"),
                        reflow_scheme)
  names(all_schemes) <- c("clinical", "basic", "code", "special", "other")
  expect_snapshot_value(all_schemes, style = "json2")
})
