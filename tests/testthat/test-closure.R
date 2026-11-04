test_that("the package closure follows Depends and Imports, not LinkingTo", {
  root <- withr::local_tempdir()
  for (package in c("fakeA", "fakeB")) {
    dir.create(file.path(root, package))
    writeLines(c(paste("Package:", package), "Version: 1.0",
                 if (package == "fakeA") c("Imports: fakeB", "LinkingTo: headeronly")),
               file.path(root, package, "DESCRIPTION"))
  }
  real_find <- find.package
  scope <- new.env(parent = environment(rf_closure))
  scope$find.package <- function(package, ...) {
    if (package %in% c("fakeA", "fakeB")) file.path(root, package)
    else real_find(package, ...)
  }
  discover <- rf_closure
  environment(discover) <- scope
  closure <- discover("fakeA")
  expect_true(all(c("fakeA", "fakeB", "reflowR", "digest") %in% closure))
  expect_false("headeronly" %in% closure)
  expect_true(all(c("fs", "cli", "rlang") %in% closure))
})

test_that("loaded package dependencies determine the recorded execution closure", {
  skip_on_cran()
  root <- normalizePath(withr::local_tempdir(), winslash = "/")
  old <- file.path(root, "old")
  new <- file.path(root, "new")
  dir.create(old)
  dir.create(new)
  withr::local_libpaths(c(new, old), action = "prefix")
  withr::local_envvar(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  install <- function(key, package, version, imports, code, library) {
    path <- file.path(root, key)
    dir.create(path)
    dir.create(file.path(path, "R"))
    writeLines(c(paste("Package:", package), paste("Version:", version),
                 "Title: Synthetic Dependency Fixture",
                 "Description: Private test fixture for package dependency identity.",
                 "License: MIT", "Author: Test Fixture",
                 "Maintainer: Test Fixture <fixture@example.invalid>",
                 if (length(imports)) paste("Imports:", imports)),
               file.path(path, "DESCRIPTION"))
    writeLines("export(value)", file.path(path, "NAMESPACE"))
    writeLines(code, file.path(path, "R", "value.R"))
    log <- file.path(root, paste0(key, ".log"))
    status <- system2(file.path(R.home("bin"), "R"), c(
      "CMD", "INSTALL", "--no-byte-compile",
      paste0("--library=", shQuote(library)), shQuote(path)
    ), stdout = log, stderr = log)
    expect_identical(status, 0L, info = paste(readLines(log), collapse = "\n"))
  }
  install("leaf-a", "reflowtestLeafA", "1.0.0", character(),
          "value <- function() 1", old)
  install("leaf-b", "reflowtestLeafB", "1.0.0", character(),
          "value <- function() 9", new)
  install("parent-old", "reflowtestParent", "1.0.0", "reflowtestLeafA",
          "value <- function() reflowtestLeafA::value()", old)
  install("parent-new", "reflowtestParent", "2.0.0", "reflowtestLeafB",
          "value <- function() reflowtestLeafB::value()", new)
  loadNamespace("reflowtestParent", lib.loc = old)
  withr::defer({
    for (package in c("reflowtestParent", "reflowtestLeafA", "reflowtestLeafB")) {
      if (isNamespaceLoaded(package)) unloadNamespace(package)
    }
  })
  expect_identical(getExportedValue("reflowtestParent", "value")(), 1)
  stage <- reflow_imaging_stage("value", "reflowtestParent", "value")
  signature <- rf_packages(list(stage))
  expect_true("reflowtestLeafA" %in% names(signature))
  expect_false("reflowtestLeafB" %in% names(signature))
  expect_identical(signature$reflowtestParent$version, "1.0.0")
  plan <- reflow_imaging_plan(stage)
  run <- file.path(root, "run")
  expect_identical(reflow_imaging_run(plan, run)$value, 1)
  expect_identical(reflow_imaging_resume(plan, run)$value, 1)
  description <- file.path(old, "reflowtestLeafA", "DESCRIPTION")
  write("X-Test-Revision: changed", description, append = TRUE)
  expect_error(reflow_imaging_resume(plan, run), "changed")
  expect_false(file.exists(file.path(run, "READY")))
})
