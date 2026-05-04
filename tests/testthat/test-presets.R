test_that("reflow_preset returns valid preset for all names", {
  for (p in c("minimal", "standard", "extended")) {
    preset <- reflow_preset(p)
    expect_true(is.list(preset))
    expect_true(all(c("name", "files", "descriptions") %in% names(preset)))
    expect_equal(length(preset$files), length(preset$descriptions))
  }
})

test_that("preset file counts are correct", {
  expect_equal(length(reflow_preset("minimal")$files), 3)
  expect_equal(length(reflow_preset("standard")$files), 8)
  expect_equal(length(reflow_preset("extended")$files), 12)
})

test_that("reflow_preset errors on unknown preset", {
  expect_error(reflow_preset("mega"))
})

test_that("reflow_presets returns all preset names", {
  result <- reflow_presets()
  expect_equal(sort(result), sort(c("minimal", "standard", "extended")))
})
