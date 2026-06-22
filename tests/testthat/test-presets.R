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

test_that("reflow_presets prints one line per preset and returns invisibly", {
  out <- capture.output(res <- reflow_presets(), type = "message")
  expect_invisible(reflow_presets())
  for (p in c("minimal", "standard", "extended")) {
    expect_true(any(grepl(p, out)))
  }
  expect_length(res, 3)
})

test_that("reflow_preset supports partial matching via match.arg", {
  expect_identical(reflow_preset("min")$name, "minimal")
})

test_that("every preset file has a matching description entry", {
  for (p in c("minimal", "standard", "extended")) {
    preset <- reflow_preset(p)
    expect_setequal(names(preset$descriptions), preset$files)
  }
})

test_that("preset definitions are stable (snapshot of all presets)", {
  all_presets <- lapply(c("minimal", "standard", "extended"), reflow_preset)
  names(all_presets) <- c("minimal", "standard", "extended")
  expect_snapshot_value(all_presets, style = "json2")
})
