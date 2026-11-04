.provenance_no_readlink <- function(fun) {
  scope <- new.env(parent = environment(fun))
  scope$Sys.readlink <- function(paths) rep("", length(paths))
  environment(fun) <- scope
  fun
}

test_that("empty directory changes invalidate recorded imaging inputs", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/")
  source <- file.path(root, "input")
  dir.create(source)
  writeLines("original", file.path(source, "a.txt"))
  withr::local_options(reflowR.package_hash_cache = TRUE)
  plan <- reflow_imaging_plan(reflow_imaging_stage(
    "listing", "base", "list.files", list(path = reflow_imaging_input(source))
  ))
  run <- file.path(root, "run")
  expect_identical(reflow_imaging_run(plan, run)$listing, "a.txt")
  expect_identical(reflow_imaging_resume(plan, run)$listing, "a.txt")
  dir.create(file.path(source, "added-empty-directory"))
  expect_error(reflow_imaging_resume(plan, run), "changed")
  expect_false(file.exists(file.path(run, "READY")))
})

test_that("cached nested symlinks track the file bytes actually consumed", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/")
  source <- file.path(root, "input")
  dir.create(source)
  target <- file.path(root, "measurement.txt")
  writeLines("old measurement", target)
  link <- file.path(source, "linked.txt")
  if (!isTRUE(suppressWarnings(file.symlink(target, link)))) {
    skip("This account/filesystem cannot create file symbolic links.")
  }
  withr::defer(fs::link_delete(link))
  withr::local_options(reflowR.package_hash_cache = TRUE,
                      reflowR.input_hash_cache = TRUE)
  plan <- reflow_imaging_plan(
    reflow_imaging_stage("paths", "base", "list.files",
                         list(path = reflow_imaging_input(source), full.names = TRUE)),
    reflow_imaging_stage("values", "base", "lapply",
                         list(X = reflow_imaging_ref("paths"), FUN = "readLines"))
  )
  run <- file.path(root, "run")
  expect_identical(reflow_imaging_run(plan, run)$values, list("old measurement"))
  expect_identical(reflow_imaging_resume(plan, run)$values, list("old measurement"))
  writeLines("a changed measurement", target)
  expect_identical(rf_input_hashes(link), rf_file_hashes(target))
  expect_error(reflow_imaging_resume(plan, run), "changed")
  expect_false(file.exists(file.path(run, "READY")))
  expect_identical(readLines(target), "a changed measurement")
})

test_that("artifact and strict link guards do not depend on readlink", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/")
  target <- file.path(root, "physical")
  dir.create(target)
  writeLines("outside payload", file.path(target, "sentinel.txt"))
  tree <- file.path(root, "tree")
  dir.create(tree)
  link <- file.path(tree, "alias")
  linked <- tryCatch({ fs::link_create(target, link); TRUE }, error = function(e) FALSE)
  if (!linked) skip("This account/filesystem cannot create a directory link/junction.")
  withr::defer(fs::link_delete(link))
  expect_true(fs::is_link(link))
  for (fun in list(rfa_no_links, strict_path_ancestors)) {
    guarded <- .provenance_no_readlink(fun)
    expect_error(guarded(file.path(link, "sentinel.txt")), "Symbolic links")
    expect_error(guarded(file.path(link, "not-created")), "Symbolic links")
  }
  expect_error(.provenance_no_readlink(rfa_tree)(tree), "Symbolic links")
  expect_error(.provenance_no_readlink(strict_tree_files)(tree), "Symbolic links")
  expect_identical(readLines(file.path(target, "sentinel.txt")), "outside payload")
})

test_that("dangling file links are refused independently of target existence", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/")
  link <- file.path(root, "dangling")
  target <- file.path(root, "absent-target")
  if (!isTRUE(suppressWarnings(file.symlink(target, link)))) {
    skip("This account/filesystem cannot create file symbolic links.")
  }
  withr::defer(fs::link_delete(link))
  expect_true(fs::is_link(link))
  expect_error(normalizePath(target, mustWork = TRUE))
  expect_error(.provenance_no_readlink(rfa_no_links)(link), "Symbolic links")
  expect_error(.provenance_no_readlink(strict_path_ancestors)(link), "Symbolic links")
  expect_error(.provenance_no_readlink(rfa_tree)(root), "Symbolic links")
  expect_error(.provenance_no_readlink(strict_tree_files)(root), "Symbolic links")
})

test_that("generated strict helpers retain standalone portable link refusal", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/")
  physical <- file.path(root, "physical")
  dir.create(physical)
  package <- file.path(physical, "cttiR")
  dir.create(package)
  writeLines(c("Package: cttiR", "Version: 0.1.0"), file.path(package, "DESCRIPTION"))
  writeLines("export(study_stage)", file.path(package, "NAMESPACE"))
  files <- c("DESCRIPTION", "NAMESPACE")
  runtime <- list(version = "0.1.0", files = stats::setNames(lapply(files,
    function(file) digest::digest(file = file.path(package, file), algo = "sha256")), files))
  helper <- new.env(parent = baseenv())
  helper$Sys.readlink <- function(paths) rep("", length(paths))
  eval(parse(text = strict_template("strict-setup.R")), envir = helper)
  eval(parse(text = strict_cttir_runtime_text(list())), envir = helper)
  expect_true(helper$.strict_cttir_runtime_status(runtime, package)$current)
  expect_length(helper$.strict_runtime_files(physical), 2L)
  alias <- file.path(root, "alias")
  linked <- tryCatch({ fs::link_create(physical, alias); TRUE }, error = function(e) FALSE)
  if (!linked) skip("This account/filesystem cannot create a directory link/junction.")
  withr::defer(fs::link_delete(alias))
  expect_true(fs::is_link(alias))
  status <- helper$.strict_cttir_runtime_status(runtime, file.path(alias, "cttiR"))
  expect_false(status$current)
  expect_match(status$reason, "ancestors must not be symbolic links")
  expect_error(helper$.strict_runtime_files(root), "Symbolic links")
})

test_that("tracked directory links preserve ordinary aliases and refuse cycles", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/")
  source <- file.path(root, "input")
  target <- file.path(root, "target")
  dir.create(source)
  dir.create(target)
  dir.create(file.path(target, "empty"))
  writeLines("literal", file.path(target, "value.txt"))
  alias <- file.path(source, "alias")
  linked <- tryCatch({ fs::link_create(target, alias); TRUE }, error = function(e) FALSE)
  if (!linked) skip("This account/filesystem cannot create a directory link/junction.")
  withr::defer(fs::link_delete(alias))
  tree <- rf_tree(source)
  expect_identical(tree$directories, c("alias", "alias/empty"))
  expect_identical(names(tree$files), "alias/value.txt")
  expect_identical(unname(tree$files), rf_file_hash(file.path(target, "value.txt")))
  cycle <- file.path(target, "back")
  fs::link_create(source, cycle)
  withr::defer(fs::link_delete(cycle))
  expect_error(rf_tree(source), "directory link cycle")
  expect_identical(readLines(file.path(target, "value.txt")), "literal")
})

test_that("imaging refuses POSIX streams before attempting to hash them", {
  skip_on_os("windows") # Native Windows R does not observe MSYS FIFO types.
  skip_if(!nzchar(Sys.which("mkfifo")), "mkfifo unavailable")
  root <- normalizePath(withr::local_tempdir(), winslash = "/")
  stream <- file.path(root, "stream")
  expect_identical(system2(Sys.which("mkfifo"), shQuote(stream)), 0L)
  expect_identical(as.character(fs::file_info(stream)$type), "FIFO")
  local_mocked_bindings(rf_input_hashes = function(...) {
    stop("hashing must not be reached for a stream")
  })
  expect_error(rf_tree(stream), "regular files or directories")
  expect_error(rf_tree(root), "regular files or directories")
})
