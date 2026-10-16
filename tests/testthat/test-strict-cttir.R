cttir_profile_fixture <- function() {
  package <- tempfile("cttir-runtime-", tmpdir = normalizePath(tempdir(), mustWork = TRUE))
  dir.create(package)
  dir.create(file.path(package, "R"))
  writeLines(c("Package: cttiR", "Version: 0.1.0"), file.path(package, "DESCRIPTION"))
  writeLines("export(study_stage)", file.path(package, "NAMESPACE"))
  writeLines("inert installed-package fixture", file.path(package, "R", "cttiR.rdb"))
  withr::defer(unlink(package, recursive = TRUE), envir = parent.frame())
  files <- sort(list.files(package, recursive = TRUE))
  runtime <- list(package = "cttiR", version = "0.1.0", writer = "study_stage",
    files = stats::setNames(lapply(files, function(file) digest::digest(file = file.path(package, file), algo = "sha256")), files))
  packages <- c("workflowr", "rmarkdown", "knitr", "digest", "jsonlite")
  lock <- list(R = list(Version = as.character(getRversion())), Packages = stats::setNames(
    lapply(packages, function(p) list(Package = p, Version = as.character(utils::packageVersion(p)), Source = "Repository")), packages))
  lock$Packages$cttiR <- list(Package = "cttiR", Version = "0.1.0", Source = "Repository")
  composition <- list(schema_version = 1L,
    study = list(schema_version = 1L, study_id = "clinical-example", unresolved = list(),
      outcome = list(name = "pressure", unit = "mmHg"), options = list(intercept = TRUE)),
    recipe = list(id = "clinical.independent_continuous_association.v1", version = "1"), runtime = runtime)
  list(package = package, composition = composition, lock = lock)
}

cttir_profile_project <- function(fixture, one_stage = FALSE) {
  overrides <- if (one_stage) stats::setNames(lapply(.strict_stage_ids, function(id)
    list(applicable = identical(id, "descriptive"), reason = if (id == "descriptive") "" else "Isolated structural provenance fixture")), .strict_stage_ids) else NULL
  profile <- reflow_profile("clinical", "secondary", "observational", profile_version = "strict-cttir-v1",
    data_access = "synthetic", jurisdiction = "Germany", legal_basis = "Generated test observations", stages = overrides)
  directory <- tempfile("cttir-project-", tmpdir = normalizePath(tempdir(), mustWork = TRUE))
  withr::defer({
    Sys.chmod(file.path(directory, "data/raw"), "0755")
    unlink(directory, recursive = TRUE)
  }, envir = parent.frame())
  reflow_init_strict(directory, profile, "Clinical fixture", "Research group", fixture$lock,
                     composition = fixture$composition)
}

test_that("composed control and stage inventory is finite and immutable", {
  fixture <- cttir_profile_fixture()
  project <- cttir_profile_project(fixture)
  expect_identical(project$profile$primary_color, "#C8102E")
  expect_identical(project$profile$badge, "SECONDARY DATA")
  expect_identical(vapply(project$profile$stages, `[[`, "", "id"), .strict_stage_ids)
  expect_true(all(.strict_cttir_controls %in% names(project$files)))
  expect_identical(project$files[["code/stages/06_primary.R"]],
    'cttiR::study_stage(stage = "primary", directory = ".")\n')
  expect_identical(reflow_scaffold(project$profile, "Clinical fixture", "Research group", fixture$lock,
    fixture$composition)$sha256, project$sha256)
  expect_error(reflow_validate_strict(project$directory, project$declaration,
    reviewed_files = c("code/stages/06_primary.R" = strrep("a", 64))), "forbids")
  for (path in c(.strict_cttir_controls, "code/stages/06_primary.R", "analysis/06_primary.Rmd", "_workflowr.yml")) {
    writeLines("changed", file.path(project$directory, path))
    expect_false(reflow_validate_strict(project$directory, project$declaration)$valid, info = path)
    writeBin(charToRaw(project$files[[path]]), file.path(project$directory, path))
  }
  dir.create(file.path(project$directory, "config"))
  writeLines("unlisted", file.path(project$directory, "config/extra.json"))
  expect_false(reflow_validate_strict(project$directory, project$declaration)$valid)
})

test_that("composition rejects unsupported dispatch and malformed or executable data", {
  fixture <- cttir_profile_fixture()
  profile <- reflow_profile("clinical", "secondary", "observational", profile_version = "strict-cttir-v1")
  build <- function(x) reflow_scaffold(profile, "Study", "Group", fixture$lock, x)
  expect_error(build(NULL), "composition")
  bad <- fixture$composition; bad$runtime$writer <- "system"
  expect_error(build(bad), "fixed")
  bad <- fixture$composition; bad$runtime$package <- "other"
  expect_error(build(bad), "fixed")
  bad <- fixture$composition; bad$runtime$version <- "9.9.9"
  expect_error(build(bad), "lock")
  bad <- fixture$composition; bad$extra_files <- list("extra.R" = "stop()")
  expect_error(build(bad), "composition")
  bad <- fixture$composition; bad$study$execute <- function() stop("must never execute")
  expect_error(build(bad), "plain JSON")
  bad <- fixture$composition; bad$study$value <- NaN
  expect_error(build(bad), "finite")
  bad <- fixture$composition; bad$study <- structure(list(1, 2), names = c("same", "same"))
  expect_error(build(bad), "keys")
  bad <- fixture$composition; bad$study$value <- paste(rep("x", 16001), collapse = "")
  expect_error(build(bad), "finite")
  bad <- fixture$composition; bad$runtime$files[["../outside"]] <- strrep("a", 64)
  expect_error(build(bad), "relative")
  bad <- fixture$composition; bad$runtime$files <- list()
  expect_error(build(bad), "complete")
  expect_error(reflow_scaffold(reflow_profile("clinical", "secondary", "observational"),
    "Study", "Group", fixture$lock, fixture$composition), "requires strict-cttir")
})

test_that("runtime identities require complete regular installed files and fixed export", {
  fixture <- cttir_profile_fixture()
  check <- function(runtime = fixture$composition$runtime) strict_cttir_runtime_status(runtime, fixture$package)
  expect_true(check()$current)
  writeLines("changed byte", file.path(fixture$package, "R", "cttiR.rdb"))
  expect_false(check()$current)
  writeLines("inert installed-package fixture", file.path(fixture$package, "R", "cttiR.rdb"))
  writeLines("extra", file.path(fixture$package, "extra"))
  expect_match(check()$reason, "inventory")
  unlink(file.path(fixture$package, "extra"))
  runtime <- fixture$composition$runtime; runtime$files[["R/cttiR.rdb"]] <- NULL
  expect_match(check(runtime)$reason, "inventory")
  writeLines("export(other_writer)", file.path(fixture$package, "NAMESPACE"))
  runtime <- fixture$composition$runtime
  runtime$files$NAMESPACE <- digest::digest(file = file.path(fixture$package, "NAMESPACE"), algo = "sha256")
  expect_match(check(runtime)$reason, "export")
  writeLines("export(study_stage)", file.path(fixture$package, "NAMESPACE"))
  skip_on_os("windows")
  expect_true(file.symlink(file.path(fixture$package, "DESCRIPTION"), file.path(fixture$package, "linked")))
  expect_match(check()$reason, "without links")
})

test_that("composed readiness and completion require runtime and current external receipts", {
  fixture <- cttir_profile_fixture()
  project <- cttir_profile_project(fixture, one_stage = TRUE)
  original <- strict_cttir_runtime_status
  local_mocked_bindings(strict_cttir_runtime_installed = function(runtime)
    original(runtime, fixture$package))
  result <- reflow_validate_strict(project$directory, project$declaration)
  expect_true(result$valid)
  expect_true(result$pristine)
  expect_true(result$execution_ready)
  expect_false(result$completed)
  stage <- project$profile$stages[[5L]]
  withr::local_dir(project$directory)
  runtime <- new.env(parent = baseenv())
  sys.source("code/strict-setup.R", runtime)
  runtime$.strict_cttir_runtime_status <- function(x, package_path = NULL) original(x, fixture$package)
  # Bypass only the synthetic package version lookup; no candidate writer executes.
  runtime$.strict_cttir_base_begin <- function(stage, seed) list(stage = stage, seed = seed,
    inputs = runtime$.strict_runtime_hashes(runtime$.strict_runtime_files("data/raw")),
    outputs_before = list(), output_times_before = numeric())
  state <- runtime$strict_stage_begin(stage$id, project$profile$seed)
  writeLines("recorded output fixture", "output/result.txt")
  runtime$strict_stage_finish(state, stage$analysis, stage$code)
  hashes <- c(descriptive = digest::digest(file = "output/provenance/descriptive.json", algo = "sha256"))
  expect_true(reflow_validate_strict(project$directory, project$declaration, execution_receipts = hashes)$completed)
  writeLines("changed output", "output/result.txt")
  expect_false(reflow_validate_strict(project$directory, project$declaration, execution_receipts = hashes)$completed)
  writeLines("recorded output fixture", "output/result.txt")
  writeLines("runtime drift", file.path(fixture$package, "R", "cttiR.rdb"))
  result <- reflow_validate_strict(project$directory, project$declaration, execution_receipts = hashes)
  expect_false(result$execution_ready)
  expect_false(result$completed)
  expect_error(runtime$strict_stage_begin(stage$id, project$profile$seed), "hash differs")
})

test_that("composed runtime refuses control changes during rendering", {
  fixture <- cttir_profile_fixture()
  project <- cttir_profile_project(fixture, one_stage = TRUE)
  withr::local_dir(project$directory)
  runtime <- new.env(parent = baseenv())
  sys.source("code/strict-setup.R", runtime)
  state <- list(composition_before = runtime$.strict_runtime_hashes(.strict_cttir_controls))
  writeLines("changed study", "cttir-study.json")
  expect_error(runtime$strict_stage_finish(state, "analysis/05_descriptive.Rmd", "code/stages/05_descriptive.R"), "controls changed")
  expect_false(file.exists("output/provenance/descriptive.json"))
})

test_that("compiled control pins reject self-repinning before any writer preflight", {
  fixture <- cttir_profile_fixture()
  project <- cttir_profile_project(fixture, one_stage = TRUE)
  withr::local_dir(project$directory)
  runtime <- new.env(parent = baseenv())
  sys.source("code/strict-setup.R", runtime)
  for (path in c(.strict_cttir_controls, "reflow-profile.json", "renv.lock")) {
    writeLines("changed before dispatch", path)
    expect_error(runtime$strict_stage_begin("descriptive", project$profile$seed), "controls changed from the compiled")
    writeBin(charToRaw(project$files[[path]]), path)
  }
  # An attacker cannot bless a changed package by replacing the runtime map.
  writeLines("changed package payload", file.path(fixture$package, "R", "cttiR.rdb"))
  forged <- fixture$composition$runtime
  forged$files[["R/cttiR.rdb"]] <- digest::digest(file = file.path(fixture$package, "R", "cttiR.rdb"), algo = "sha256")
  writeBin(charToRaw(strict_json(forged)), "cttir-runtime.json")
  expect_error(runtime$strict_stage_begin("descriptive", project$profile$seed), "controls changed from the compiled")
  expect_false(file.exists("output/provenance/descriptive.json"))
})
