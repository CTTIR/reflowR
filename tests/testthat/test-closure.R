test_that("the package closure follows Depends and Imports, not LinkingTo", {
  db <- cbind(Package = c("fakeA", "fakeB"), Version = "1.0", Depends = NA,
              Imports = c("fakeB", NA), LinkingTo = c("headeronly", NA))
  rownames(db) <- db[, "Package"]
  closure <- rf_closure("fakeA", db)
  expect_true(all(c("fakeA", "fakeB", "reflowR", "digest") %in% closure))
  expect_false("headeronly" %in% closure)
})
