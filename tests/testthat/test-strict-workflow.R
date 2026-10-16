strict_test_lock <- function() {
  packages <- c("workflowr", "rmarkdown", "knitr", "digest", "jsonlite")
  list(R = list(Version = as.character(getRversion())), Packages = stats::setNames(
    lapply(packages, function(p) list(Package = p, Version = as.character(utils::packageVersion(p)), Source = "Repository")), packages))
}

strict_test_project <- function(profile = reflow_profile("clinical", "secondary", "observational")) {
  root <- tempfile("strict-project-", tmpdir = normalizePath(tempdir(), mustWork = TRUE))
  withr::defer({
    Sys.chmod(file.path(root, "data/raw"), "0755")
    unlink(root, recursive = TRUE)
  }, envir = parent.frame())
  reflow_init_strict(root, profile, "Study example", "Research group", strict_test_lock())
}

test_that("initialization rejects a linked ancestor before creating project files", {
  skip_on_os("windows")
  parent <- tempfile("strict-parent-", tmpdir = normalizePath(tempdir(), mustWork = TRUE))
  dir.create(parent)
  withr::defer(unlink(parent, recursive = TRUE))
  real <- file.path(parent, "physical")
  link <- file.path(parent, "linked")
  dir.create(real)
  expect_true(file.symlink(real, link))
  profile <- reflow_profile("clinical", "secondary", "observational")
  expect_error(reflow_init_strict(file.path(link, "project"), profile,
    "Study example", "Research group", strict_test_lock()), "Symbolic links")
  expect_false(dir.exists(file.path(real, "project")))
})

test_that("domain, data origin and research design retain explicit palette semantics", {
  p <- reflow_profile("clinical", "secondary", "observational")
  expect_identical(p$primary_color, "#C8102E")
  expect_identical(p$badge, "SECONDARY DATA")
  expect_identical(reflow_profile("basic", "primary", "experimental")$primary_color, "#4682B4")
  expect_identical(reflow_profile("clinical", "secondary", "synthesis")$primary_color, "#6A0DAD")
  expect_identical(reflow_profile("basic", "primary", "simulation")$primary_color, "#228B22")
  expect_length(p$stages, 11L)
  expect_false(any(grepl("PCA|dimension", vapply(p$stages, `[[`, "", "id"))))
  expect_error(reflow_profile("clinical", "secondary", "observational", stages = list(primary = list(applicable = FALSE, reason = ""))), "nonempty")
  p$primary_color <- "#ffffff"
  expect_error(reflow_scaffold(p, "Study", "Group", strict_test_lock()), "canonical")
})

test_that("initialization is inert and pristine structure does not imply readiness", {
  s <- strict_test_project()
  expect_false(dir.exists(file.path(s$directory, ".git")))
  result <- reflow_validate_strict(s$directory, s$declaration)
  expect_true(result$valid)
  expect_true(result$pristine)
  expect_false(result$execution_ready)
  expect_false(result$completed)
  expect_error(reflow_init_strict(s$directory, s$profile, "Study", "Group", strict_test_lock()), "new directory")
  lock <- strict_test_lock(); lock$Packages$workflowr$Version <- NULL
  expect_error(reflow_scaffold(s$profile, "Study", "Group", lock), "versions")
})

test_that("control files cannot approve themselves or be externally waived", {
  s <- strict_test_project()
  path <- file.path(s$directory, "_workflowr.yml")
  cat("seed: 999\n", file = path)
  expect_false(reflow_validate_strict(s$directory, s$declaration)$valid)
  hashes <- stats::setNames(digest::digest(file = path, algo = "sha256"), "_workflowr.yml")
  expect_error(reflow_validate_strict(s$directory, s$declaration, hashes), "Only declared")
  for (path in c("../outside.R", "/tmp/outside.R", "code//bad.R", "code/./bad.R")) {
    expect_error(reflow_validate_strict(s$directory, s$declaration, stats::setNames(strrep("a", 64), path)), "relative paths")
  }
})

test_that("reviewed stage edits must preserve the canonical workflow envelope", {
  s <- strict_test_project()
  stage <- s$profile$stages[[1L]]
  path <- file.path(s$directory, stage$analysis)
  text <- s$files[[stage$analysis]]
  text <- sub("Record the planned question, inputs, decisions and assumptions.", "Prespecified protocol with reviewed inputs and assumptions.", text, fixed = TRUE)
  writeBin(charToRaw(text), path)
  expect_false(reflow_validate_strict(s$directory, s$declaration)$valid)
  hashes <- stats::setNames(digest::digest(file = path, algo = "sha256"), stage$analysis)
  expect_true(reflow_validate_strict(s$directory, s$declaration, hashes)$valid)
  writeBin(charToRaw(sub("strict_stage_begin", "unreviewed_start", text, fixed = TRUE)), path)
  hashes[[stage$analysis]] <- digest::digest(file = path, algo = "sha256")
  expect_false(reflow_validate_strict(s$directory, s$declaration, hashes)$valid)
})

test_that("unknown executable entrypoints and nested roots fail closed", {
  for (relative in c("publications/child/.Rprofile", "publications/child/renv.lock", "output/unreviewed.R", "unreviewed.py")) {
    s <- strict_test_project()
    dir.create(dirname(file.path(s$directory, relative)), recursive = TRUE, showWarnings = FALSE)
    writeLines("unreviewed", file.path(s$directory, relative))
    expect_false(reflow_validate_strict(s$directory, s$declaration)$valid, info = relative)
  }
})

test_that("raw permissions and symbolic links are checked without following links", {
  skip_on_os("windows")
  s <- strict_test_project()
  Sys.chmod(file.path(s$directory, "data/raw"), "0755")
  expect_false(reflow_validate_strict(s$directory, s$declaration)$valid)
  Sys.chmod(file.path(s$directory, "data/raw"), "0555")
  expect_true(file.symlink(tempdir(), file.path(s$directory, "output/escape")))
  expect_false(reflow_validate_strict(s$directory, s$declaration)$valid)
})

test_that("known legal declarations alone do not imply implemented or completed work", {
  p <- reflow_profile("basic", "primary", "simulation", data_access = "synthetic",
                      jurisdiction = "Germany", legal_basis = "Generated teaching data without personal records")
  s <- strict_test_project(p)
  result <- reflow_validate_strict(s$directory, s$declaration)
  expect_true(result$legal_declarations_complete)
  expect_false(result$execution_ready)
  expect_false(result$completed)
  expect_error(reflow_validate_strict(s$directory, s$declaration,
    execution_receipts = c(unlisted_stage = strrep("a", 64))), "Unknown execution")
})

test_that("completion requires current stage outputs and externally retained receipts", {
  ids <- reflow_profile("basic", "primary", "simulation")$stages
  inactive <- stats::setNames(lapply(ids, function(x) list(applicable = FALSE,
    reason = "Single-stage technical test")), vapply(ids, `[[`, "", "id"))
  inactive$descriptive <- list(applicable = TRUE, reason = "")
  p <- reflow_profile("basic", "primary", "simulation", stages = inactive,
    data_access = "synthetic", jurisdiction = "Germany", legal_basis = "Generated test values")
  s <- strict_test_project(p)
  stage <- p$stages[[5L]]
  code <- 'utils::write.csv(data.frame(value = c(1, 3, 5)), "output/summary.csv", row.names = FALSE)\n'
  writeBin(charToRaw(code), file.path(s$directory, stage$code))
  writeBin(charToRaw(sub("Record the planned question, inputs, decisions and assumptions.",
    "Summarize generated test values.", s$files[[stage$analysis]], fixed = TRUE)), file.path(s$directory, stage$analysis))
  reviewed <- stats::setNames(vapply(c(stage$analysis, stage$code), function(path)
    digest::digest(file = file.path(s$directory, path), algo = "sha256"), ""), c(stage$analysis, stage$code))
  expect_true(reflow_validate_strict(s$directory, s$declaration, reviewed)$execution_ready)
  withr::local_dir(s$directory)
  source("code/strict-setup.R", local = TRUE)
  writeLines("unrelated earlier artifact", "output/earlier.txt")
  state <- strict_stage_begin(stage$id, p$seed)
  no_output <- strict_stage_finish(state, stage$analysis, stage$code)
  expect_length(no_output$outputs, 0L)
  source(stage$code, local = TRUE)
  strict_stage_finish(state, stage$analysis, stage$code)
  receipt <- c(descriptive = digest::digest(file = "output/provenance/descriptive.json", algo = "sha256"))
  expect_true(reflow_validate_strict(s$directory, s$declaration, reviewed, receipt)$completed)
  writeLines("changed result", "output/summary.csv")
  expect_false(reflow_validate_strict(s$directory, s$declaration, reviewed, receipt)$completed)
})
