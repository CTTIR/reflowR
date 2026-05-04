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
