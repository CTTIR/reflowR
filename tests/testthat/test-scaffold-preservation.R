.scaffold_snapshot <- function(root) {
  paths <- fs::dir_ls(root, all = TRUE, recurse = TRUE, type = "file")
  names <- as.character(fs::path_rel(paths, root))
  stats::setNames(lapply(paths, function(path) list(
    hash = digest::digest(file = path, algo = "sha256"),
    mode = as.integer(file.info(path)$mode)
  )), names)
}

.scaffold_project <- function(root, name = "project") {
  path <- file.path(root, name)
  suppressMessages(reflow_init(path, author = "Original", depth = "minimal",
                               git = FALSE, change_wd = FALSE, open = FALSE))
  writeLines("unrelated original data", file.path(path, "user-data.txt"))
  path
}

.scaffold_retheme <- function(path, ...) {
  suppressMessages(reflow_init(path, author = "Replacement", depth = "minimal",
                               existing = TRUE, overwrite = TRUE, git = FALSE,
                               change_wd = FALSE, open = FALSE, ...))
}

test_that("invalid scalar metadata fails before a scaffold is started", {
  root <- withr::local_tempdir()
  target <- file.path(root, "project")
  called <- FALSE
  local_mocked_bindings(reflow_init_start = function(...) {
    called <<- TRUE
    cli::cli_abort("generator should not run")
  })
  bad <- list(author = c("A", "B"), name = NA_character_,
              email = character(), affiliation = list("x"),
              orcid = "line\nbreak", website = 1, github_url = c("a", "b"),
              seed = Inf, existing = NA, git = 1, change_wd = c(TRUE, FALSE),
              open = NULL)
  for (key in names(bad)) {
    args <- list(directory = target, git = FALSE, open = FALSE,
                 change_wd = FALSE)
    args[key] <- bad[key]
    expect_error(do.call(reflow_init, args), key)
    expect_false(called)
    expect_false(dir.exists(target))
  }
  for (seed in list(-1, 0.5, 2^31, NA_real_, 1i)) {
    expect_error(reflow_init(target, seed = seed), "seed")
  }
  expect_error(reflow_init(target, user.name = "User"), "both")
})

test_that("failed generation preserves the complete existing project and Git index", {
  root <- withr::local_tempdir()
  path <- .scaffold_project(root)
  repo <- git2r::init(path)
  git2r::config(repo, user.name = "Fixture", user.email = "fixture@example.invalid")
  git2r::add(repo, "user-data.txt")
  git2r::commit(repo, "Fixture base")
  writeLines("staged user data", file.path(path, "user-data.txt"))
  git2r::add(repo, "user-data.txt")
  writeLines("unstaged user data", file.path(path, "user-data.txt"))
  dir.create(file.path(path, "empty-user-directory"))
  before <- .scaffold_snapshot(path)
  root_inode <- fs::file_info(path)$inode
  local_mocked_bindings(generate_project_readme = function(...) {
    cli::cli_abort("injected final generator failure")
  })
  expect_error(suppressMessages(reflow_init(path, author = "Changed",
    depth = "minimal", existing = TRUE, overwrite = TRUE, git = TRUE,
    open = FALSE, change_wd = FALSE)), "injected final generator")
  expect_identical(.scaffold_snapshot(path), before)
  expect_identical(fs::file_info(path)$inode, root_inode)
  expect_true(dir.exists(file.path(path, "empty-user-directory")))
  expect_false(any(startsWith(list.files(root, all.files = TRUE), ".reflow-stage-")))
  target <- file.path(root, "new-parent", "new-project")
  expect_error(suppressMessages(reflow_init(target, git = FALSE,
    open = FALSE, change_wd = FALSE)), "injected final generator")
  expect_false(dir.exists(file.path(root, "new-parent")))
})

test_that("managed links are refused before any existing data changes", {
  root <- withr::local_tempdir()
  path <- .scaffold_project(root)
  outside <- file.path(root, "outside.txt")
  writeLines("external sentinel", outside)
  readme <- file.path(path, "README.md")
  unlink(readme)
  linked <- file.symlink(outside, readme)
  skip_if_not(linked, "File symlinks unavailable on this host")
  before <- .scaffold_snapshot(path)
  expect_error(.scaffold_retheme(path), "symbolic link")
  expect_identical(readLines(outside), "external sentinel")
  expect_true(unname(fs::is_link(readme)))
  expect_identical(.scaffold_snapshot(path), before)
  unlink(readme)
  expect_true(file.symlink(file.path(root, "missing"), readme))
  expect_error(.scaffold_retheme(path), "symbolic link")
  expect_false(file.exists(file.path(root, "missing")))
  unlink(readme)
  alias <- file.path(root, "project-alias")
  expect_true(file.symlink(path, alias))
  expect_error(.scaffold_retheme(alias), "symbolic link")
  unlink(alias)
  analysis <- file.path(path, "analysis")
  moved <- file.path(root, "outside-analysis")
  expect_true(file.rename(analysis, moved))
  expect_true(file.symlink(moved, analysis))
  outside_before <- .scaffold_snapshot(moved)
  expect_error(.scaffold_retheme(path), "symbolic link")
  expect_identical(.scaffold_snapshot(moved), outside_before)
})

test_that("a failed publication restores bytes, modes and unrelated entries", {
  root <- withr::local_tempdir()
  path <- .scaffold_project(root)
  Sys.chmod(file.path(path, "README.md"), "0600")
  before <- .scaffold_snapshot(path)
  root_inode <- fs::file_info(path)$inode
  failed <- FALSE
  local_mocked_bindings(reflow_init_rename = function(from, to) {
    if (!failed && basename(from) == "about.Rmd" &&
        grepl(".reflow-stage-", from, fixed = TRUE)) {
      failed <<- TRUE
      return(FALSE)
    }
    file.rename(from, to)
  })
  expect_error(.scaffold_retheme(path), "Cannot publish scaffold file")
  expect_true(failed)
  expect_identical(.scaffold_snapshot(path), before)
  expect_identical(fs::file_info(path)$inode, root_inode)
  expect_false(any(startsWith(list.files(root, all.files = TRUE), ".reflow-stage-")))
})

test_that("uncompleted rollback retains named original backups", {
  root <- withr::local_tempdir()
  path <- .scaffold_project(root)
  original <- readBin(file.path(path, "analysis", "about.Rmd"), "raw", n = 1e6)
  local_mocked_bindings(reflow_init_rename = function(from, to) {
    if (basename(to) == "about.Rmd") return(FALSE)
    file.rename(from, to)
  })
  error <- tryCatch(.scaffold_retheme(path), error = identity)
  expect_s3_class(error, "reflow_init_recovery_error")
  expect_true(dir.exists(error$recovery_path))
  expect_match(conditionMessage(error), error$recovery_path, fixed = TRUE)
  backups <- fs::dir_ls(file.path(error$recovery_path, "backup"), type = "file")
  expect_true(any(vapply(backups, function(file) {
    identical(readBin(file, "raw", n = 1e6), original)
  }, logical(1))))
  expect_identical(readLines(file.path(path, "user-data.txt")), "unrelated original data")
})

test_that("successful replacement preserves private modes and ordinary content", {
  root <- withr::local_tempdir()
  path <- .scaffold_project(root)
  readme <- file.path(path, "README.md")
  Sys.chmod(readme, "0600")
  mode <- file.info(readme)$mode
  .scaffold_retheme(path)
  expect_identical(file.info(readme)$mode, mode)
  expect_match(paste(readLines(readme), collapse = "\n"), "Replacement", fixed = TRUE)
  expect_identical(readLines(file.path(path, "user-data.txt")), "unrelated original data")
  expect_false(any(startsWith(list.files(root, all.files = TRUE), ".reflow-stage-")))
})

test_that("a failed Git commit restores the previous index without adding user files", {
  root <- withr::local_tempdir()
  path <- .scaffold_project(root)
  repo <- git2r::init(path)
  git2r::config(repo, user.name = "Fixture", user.email = "fixture@example.invalid")
  git2r::add(repo, "user-data.txt")
  git2r::commit(repo, "Fixture base")
  index <- file.path(path, ".git", "index")
  before <- readBin(index, "raw", n = file.info(index)$size)
  head <- git2r::sha(git2r::repository_head(repo))
  writeLines("untracked user file", file.path(path, "user-extra.txt"))
  testthat::local_mocked_bindings(commit = function(...) {
    cli::cli_abort("injected Git commit failure")
  }, .package = "git2r")
  expect_warning(suppressMessages(reflow_init(path, author = "Changed",
    depth = "minimal", existing = TRUE, overwrite = TRUE, git = TRUE,
    open = FALSE, change_wd = FALSE)), "previous index was restored")
  expect_identical(readBin(index, "raw", n = file.info(index)$size), before)
  expect_identical(git2r::sha(git2r::repository_head(repo)), head)
  expect_identical(readLines(file.path(path, "user-extra.txt")), "untracked user file")
  expect_match(paste(readLines(file.path(path, "README.md")), collapse = "\n"), "Changed")
  expect_length(git2r::status(repo, unstaged = FALSE, untracked = FALSE)$staged, 0L)
})

test_that("a managed file changed during staging is preserved without publishing", {
  root <- withr::local_tempdir()
  path <- .scaffold_project(root)
  before <- .scaffold_snapshot(path)
  original_generator <- generate_project_readme
  local_mocked_bindings(generate_project_readme = function(...) {
    original_generator(...)
    writeLines("new independent edit", file.path(path, "README.md"))
  })
  expect_error(.scaffold_retheme(path), "changed during scaffold generation")
  expect_identical(readLines(file.path(path, "README.md")), "new independent edit")
  after <- .scaffold_snapshot(path)
  expect_identical(after[setdiff(names(after), "README.md")],
                   before[setdiff(names(before), "README.md")])
})

test_that("a root replaced by a link during generation is not published into", {
  root <- withr::local_tempdir()
  path <- .scaffold_project(root)
  moved <- file.path(root, "moved-project")
  before <- .scaffold_snapshot(path)
  original_generator <- generate_project_readme
  local_mocked_bindings(generate_project_readme = function(...) {
    original_generator(...)
    expect_true(file.rename(path, moved))
    expect_true(file.symlink(moved, path))
  })
  expect_error(.scaffold_retheme(path), "symbolic link")
  expect_true(unname(fs::is_link(path)))
  expect_identical(.scaffold_snapshot(moved), before)
})

test_that("a replaced existing parent prevents new-root publication", {
  root <- withr::local_tempdir()
  parent <- file.path(root, "parent")
  dir.create(parent)
  writeLines("existing parent payload", file.path(parent, "sentinel"))
  moved <- file.path(root, "moved-parent")
  target <- file.path(parent, "new-project")
  original_generator <- generate_project_readme
  local_mocked_bindings(generate_project_readme = function(...) {
    original_generator(...)
    expect_true(file.rename(parent, moved))
    expect_true(file.symlink(moved, parent))
  })
  expect_error(suppressMessages(reflow_init(target, git = FALSE,
    change_wd = FALSE, open = FALSE)), "parent or staging directory changed")
  expect_false(dir.exists(file.path(moved, "new-project")))
  expect_identical(readLines(file.path(moved, "sentinel")), "existing parent payload")
  expect_true(unname(fs::is_link(parent)))
  expect_false(any(startsWith(list.files(moved, all.files = TRUE), ".reflow-stage-")))
})

test_that("replacement staging aliases are neither traversed nor cleaned", {
  root <- withr::local_tempdir()
  path <- .scaffold_project(root)
  before <- .scaffold_snapshot(path)
  foreign <- file.path(root, "foreign")
  dir.create(foreign)
  writeLines("foreign sentinel", file.path(foreign, "sentinel"))
  moved <- file.path(root, "moved-staging")
  original_generator <- generate_project_readme
  local_mocked_bindings(generate_project_readme = function(project_dir, ...) {
    original_generator(project_dir, ...)
    stage <- dirname(project_dir)
    expect_true(file.rename(stage, moved))
    expect_true(file.symlink(foreign, stage))
  })
  error <- NULL
  expect_warning(error <- tryCatch(.scaffold_retheme(path), error = identity),
                 "Staging identity changed; no cleanup")
  expect_s3_class(error, "error")
  expect_match(conditionMessage(error), "parent or staging directory changed")
  expect_identical(.scaffold_snapshot(path), before)
  expect_identical(readLines(file.path(foreign, "sentinel")), "foreign sentinel")
  expect_true(dir.exists(moved))
})

test_that("next steps use the actual project basename and explicit project path", {
  root <- withr::local_tempdir()
  path <- file.path(root, "directory-name")
  withr::local_options(list(cli.width = 10000, cli.num_colors = 1))
  messages <- character()
  withCallingHandlers(reflow_init(path, name = "Different title", git = FALSE,
    change_wd = FALSE, open = FALSE), message = function(condition) {
      messages <<- c(messages, conditionMessage(condition))
      invokeRestart("muffleMessage")
    })
  output <- paste(messages, collapse = "\n")
  expect_match(output, "directory-name.Rproj", fixed = TRUE)
  expect_false(grepl("Different title.Rproj", output, fixed = TRUE))
  expect_match(output, "workflowr::wflow_build(project =", fixed = TRUE)
  expect_match(output, normalizePath(path), fixed = TRUE)
  expect_match(output, "workflowr::wflow_publish", fixed = TRUE)
  expect_match(output, file.path(normalizePath(path), "analysis", "*.Rmd"),
               fixed = TRUE)
  expect_true(file.exists(file.path(path, "directory-name.Rproj")))
})
