test_that("reflow_init creates workflowr-compatible project", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "test_project")
  reflow_init(
    directory = proj, author = "Test Author",
    scheme = "brand", depth = "standard",
    git = FALSE, change_wd = FALSE, open = FALSE
  )
  # workflowr base files
  expect_true(file.exists(file.path(proj, "_workflowr.yml")))
  expect_true(file.exists(file.path(proj, ".Rprofile")))
  expect_true(file.exists(file.path(proj, "test_project.Rproj")))

  # reflowR additions
  expect_true(file.exists(file.path(proj, "analysis", "theme.css")))
  expect_true(file.exists(file.path(proj, "assets", "_theme.scss")))
  expect_true(file.exists(file.path(proj, "assets", "icon.svg")))
  expect_true(file.exists(file.path(proj, "code", "00_setup.R")))
  expect_true(file.exists(file.path(proj, "code", "utils.R")))
  expect_true(file.exists(file.path(proj, "code", "formulary.R")))
  expect_true(file.exists(file.path(proj, "code", "data_io.R")))
  expect_true(file.exists(file.path(proj, "code", "table_helpers.R")))
  expect_true(file.exists(file.path(proj, "code", "plot_helpers.R")))
  expect_true(file.exists(file.path(proj, "report", "report.qmd")))

  # _site.yml uses workflowr::wflow_html
  site_yml <- readLines(file.path(proj, "analysis", "_site.yml"))
  expect_true(any(grepl("workflowr::wflow_html", site_yml)))
  expect_true(any(grepl("theme.css", site_yml)))
})

test_that("minimal preset creates 3 analysis files", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "min_test")
  reflow_init(directory = proj, author = "A", scheme = "platform",
              depth = "minimal", git = FALSE, change_wd = FALSE, open = FALSE)
  rmds <- list.files(file.path(proj, "analysis"), pattern = "^\\d+.*\\.Rmd$")
  expect_equal(length(rmds), 3)
})

test_that("standard preset creates 8 analysis files", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "std_test")
  reflow_init(directory = proj, author = "A", scheme = "brand",
              depth = "standard", git = FALSE, change_wd = FALSE, open = FALSE)
  rmds <- list.files(file.path(proj, "analysis"), pattern = "^\\d+.*\\.Rmd$")
  expect_equal(length(rmds), 8)
})

test_that("extended preset creates 12 analysis files", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "ext_test")
  reflow_init(directory = proj, author = "A", scheme = "clinical",
              depth = "extended", git = FALSE, change_wd = FALSE, open = FALSE)
  rmds <- list.files(file.path(proj, "analysis"), pattern = "^\\d+.*\\.Rmd$")
  expect_equal(length(rmds), 12)
})

test_that("all v5 families produce the v5 CSS and their family icon", {
  skip_on_cran()
  for (s in c("spectral", "spatial", "omics", "clinical", "methods",
              "simulation", "evidence", "platform", "brand")) {
    tmp <- withr::local_tempdir()
    proj <- file.path(tmp, paste0("test_", s))
    reflow_init(directory = proj, author = "A", scheme = s,
                depth = "minimal", git = FALSE, change_wd = FALSE, open = FALSE)
    css <- readLines(file.path(proj, "analysis", "theme.css"))
    expect_true(any(grepl("--cttir-page", css, fixed = TRUE)))
    expect_true(any(grepl(paste0("--cttir-topic: ", reflow_scheme(s)$accent, ";"), css, fixed = TRUE)))
    icon <- system.file("icons", paste0("icon_", s, ".svg"), package = "reflowR")
    for (copy in c("assets/icon.svg", "docs/assets/icon.svg")) {
      expect_identical(unname(tools::md5sum(file.path(proj, copy))), unname(tools::md5sum(icon)))
    }
  }
})

test_that("reflow_init defaults to the brand family", {
  expect_identical(eval(formals(reflow_init)$scheme)[1], "brand")
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "default_scheme")
  reflow_init(directory = proj, author = "A", depth = "minimal",
              git = FALSE, change_wd = FALSE, open = FALSE)
  css <- readLines(file.path(proj, "analysis", "theme.css"))
  expect_true(any(grepl("--cttir-topic: #0072B2;", css, fixed = TRUE)))
  readme <- readLines(file.path(proj, "README.md"))
  expect_true(any(grepl("Science, connected", readme, fixed = TRUE)))
})

test_that("earlier scheme names still create projects in their v5 family", {
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "alias_proj")
  suppressMessages(reflow_init(directory = proj, author = "A", scheme = "code",
                               depth = "minimal", git = FALSE, change_wd = FALSE,
                               open = FALSE))
  css <- readLines(file.path(proj, "analysis", "theme.css"))
  expect_true(any(grepl("--cttir-topic: #56B4E9;", css, fixed = TRUE)))
  icon <- system.file("icons", "icon_methods.svg", package = "reflowR")
  expect_identical(unname(tools::md5sum(file.path(proj, "assets", "icon.svg"))),
                   unname(tools::md5sum(icon)))
})

test_that("the generated _site.yml is valid and renders the site into docs/", {
  skip_if_not_installed("yaml")
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "yaml_proj")
  reflow_init(directory = proj, author = "A", scheme = "evidence", depth = "standard",
              github_url = "https://github.com/foo/bar", website = "https://example.org",
              git = FALSE, change_wd = FALSE, open = FALSE)
  site <- yaml::read_yaml(file.path(proj, "analysis", "_site.yml"))
  expect_identical(site$output_dir, "../docs")
  expect_match(site$navbar$title, '<img src="assets/icon.svg"', fixed = TRUE)
  expect_true(file.exists(file.path(proj, "docs", "assets", "icon.svg")))
  expect_length(site$navbar$left[[2]]$menu, 8L)
  expect_identical(vapply(site$navbar$right, `[[`, "", "href"),
                   c("https://github.com/foo/bar", "https://example.org"))
  expect_identical(site$output$`workflowr::wflow_html`$css, "theme.css")
  # The stylesheet sits next to the pages, so it is copied into docs/ and
  # still applies after the site is published.
  expect_true(file.exists(file.path(proj, "analysis", site$output$`workflowr::wflow_html`$css)))
})

test_that("reflow_init errors on invalid scheme", {
  tmp <- withr::local_tempdir()
  expect_error(
    reflow_init(directory = file.path(tmp, "bad"), author = "A",
                scheme = "neon", git = FALSE, change_wd = FALSE, open = FALSE)
  )
})

test_that("reflow_init derives project name from directory basename when name is NULL", {
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "derived_name_proj")
  reflow_init(directory = proj, name = NULL, author = "A",
              scheme = "brand", depth = "minimal",
              git = FALSE, change_wd = FALSE, open = FALSE)
  # the derived name appears in the generated index.Rmd title
  idx <- readLines(file.path(proj, "analysis", "index.Rmd"), warn = FALSE)
  expect_true(any(grepl("derived_name_proj", idx, fixed = TRUE)))
  # and the .Rproj is named after the basename
  expect_true(file.exists(file.path(proj, "derived_name_proj.Rproj")))
})

test_that("reflow_init passes github_url and website into the navbar", {
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "nav_proj")
  reflow_init(directory = proj, author = "A", scheme = "methods", depth = "minimal",
              github_url = "https://github.com/foo/bar",
              website = "https://example.org",
              git = FALSE, change_wd = FALSE, open = FALSE)
  yml <- readLines(file.path(proj, "analysis", "_site.yml"), warn = FALSE)
  expect_true(any(grepl("https://github.com/foo/bar", yml, fixed = TRUE)))
  expect_true(any(grepl("https://example.org", yml, fixed = TRUE)))
})

test_that("reflow_init with git = TRUE creates a repository and initial commit", {
  skip_if_not_installed("git2r")
  configured <- git2r::config(global = TRUE)
  skip_if(is.null(configured$user.name) || is.null(configured$user.email),
          "A configured user identity is required for the temporary commit test")
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "git_proj")
  reflow_init(directory = proj, author = "A", scheme = "brand", depth = "minimal",
              git = TRUE, change_wd = FALSE, open = FALSE)
  expect_true(dir.exists(file.path(proj, ".git")))
  repo <- git2r::repository(proj)
  commits <- git2r::commits(repo)
  expect_gte(length(commits), 1L)
  expect_match(commits[[1]]$message, "reflowR")
})

# A folder with the user's own repository, a committed script with an
# unstaged edit, and untracked data and notes.
user_repo <- function(proj) {
  dir.create(file.path(proj, "data", "raw"), recursive = TRUE)
  writeLines("id,value\n1,2", file.path(proj, "data", "raw", "private.csv"))
  writeLines("draft", file.path(proj, "notes.md"))
  repo <- git2r::init(proj)
  git2r::config(repo, user.name = "T", user.email = "t@example.org")
  writeLines("x <- 1", file.path(proj, "old.R"))
  git2r::add(repo, file.path(proj, "old.R"))
  git2r::commit(repo, "User commit")
  writeLines("x <- 2", file.path(proj, "old.R"))
  repo
}

test_that("git = TRUE in an existing folder commits only the files reflow_init writes", {
  skip_if_not_installed("git2r")
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "own_repo")
  repo <- user_repo(proj)
  suppressMessages(reflow_init(directory = proj, author = "A", depth = "minimal",
                               existing = TRUE, git = TRUE, change_wd = FALSE,
                               open = FALSE, user.name = "T",
                               user.email = "t@example.org"))
  expect_identical(vapply(git2r::commits(repo), `[[`, "", "message"),
                   c("Initial reflowR project", "User commit"))
  tree <- git2r::ls_tree(repo = repo, recursive = TRUE)
  committed <- paste0(tree$path, tree$name)
  expect_false(any(c("data/raw/private.csv", "notes.md") %in% committed))
  expect_true(all(c("README.md", "analysis/_site.yml", "analysis/theme.css",
                    "code/00_setup.R", ".gitignore", "own_repo.Rproj") %in% committed))
  status <- git2r::status(repo, all_untracked = TRUE)
  expect_length(status$staged, 0L)
  expect_identical(unname(unlist(status$unstaged)), "old.R")
  expect_setequal(unname(unlist(status$untracked)), c("data/raw/private.csv", "notes.md"))
})

test_that("git = TRUE makes no commit on top of changes the user has staged", {
  skip_if_not_installed("git2r")
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "staged_repo")
  repo <- user_repo(proj)
  git2r::add(repo, file.path(proj, "old.R"))
  expect_warning(
    suppressMessages(reflow_init(directory = proj, author = "A", depth = "minimal",
                                 existing = TRUE, git = TRUE, change_wd = FALSE,
                                 open = FALSE, user.name = "T",
                                 user.email = "t@example.org")),
    "staged changes"
  )
  expect_length(git2r::commits(repo), 1L)
  expect_identical(unname(unlist(git2r::status(repo)$staged)), "old.R")
})

test_that("git = TRUE commits every file of a new project", {
  skip_if_not_installed("git2r")
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "new_repo")
  suppressMessages(reflow_init(directory = proj, author = "A", depth = "minimal",
                               git = TRUE, change_wd = FALSE, open = FALSE,
                               user.name = "T", user.email = "t@example.org"))
  repo <- git2r::repository(proj)
  expect_length(git2r::commits(repo), 1L)
  status <- git2r::status(repo, all_untracked = TRUE)
  expect_length(unlist(status), 0L)
})

test_that("reflow_init writes _workflowr.yml with the requested seed", {
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "seed_proj")
  reflow_init(directory = proj, author = "A", scheme = "brand", depth = "minimal",
              seed = 12345L, git = FALSE, change_wd = FALSE, open = FALSE)
  wf <- readLines(file.path(proj, "_workflowr.yml"), warn = FALSE)
  expect_true(any(grepl("seed: 12345", wf, fixed = TRUE)))
})

test_that("reflow_init errors on invalid depth", {
  tmp <- withr::local_tempdir()
  expect_error(
    reflow_init(directory = file.path(tmp, "baddepth"), author = "A",
                depth = "ultra", git = FALSE, change_wd = FALSE, open = FALSE)
  )
})

test_that("existing = TRUE works on pre-existing directory", {
  skip_on_cran()
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "existing_proj")
  fs::dir_create(proj)
  writeLines("hello", file.path(proj, "my_data.csv"))
  reflow_init(directory = proj, author = "A", existing = TRUE,
              git = FALSE, change_wd = FALSE, open = FALSE)
  expect_true(file.exists(file.path(proj, "my_data.csv")))
  expect_true(file.exists(file.path(proj, "_workflowr.yml")))
  expect_true(file.exists(file.path(proj, "analysis", "theme.css")))
})

test_that("a repeated call on an existing project changes nothing", {
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "again_proj")
  reflow_init(directory = proj, author = "A", scheme = "brand", depth = "minimal",
              git = FALSE, change_wd = FALSE, open = FALSE)
  files <- list.files(proj, recursive = TRUE, all.files = TRUE, full.names = TRUE)
  before <- tools::md5sum(files)
  expect_error(
    reflow_init(directory = proj, author = "B", scheme = "clinical",
                depth = "minimal", existing = TRUE, git = FALSE,
                change_wd = FALSE, open = FALSE),
    "nothing was changed"
  )
  expect_identical(tools::md5sum(list.files(proj, recursive = TRUE,
                                            all.files = TRUE, full.names = TRUE)),
                   before)
})

test_that("existing = TRUE leaves the files of a workflowr project unchanged", {
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "wflow_proj")
  workflowr::wflow_start(proj, git = FALSE, change_wd = FALSE)
  own <- file.path(proj, reflow_init_replaced)
  for (f in own) cat("Written by the user\n", file = f, append = TRUE)
  listing <- function() {
    tools::md5sum(list.files(proj, recursive = TRUE, all.files = TRUE,
                             full.names = TRUE))
  }
  before <- listing()
  err <- expect_error(
    reflow_init(directory = proj, author = "A", depth = "minimal",
                existing = TRUE, git = FALSE, change_wd = FALSE, open = FALSE),
    "nothing was changed"
  )
  for (f in reflow_init_replaced) expect_match(conditionMessage(err), f, fixed = TRUE)
  expect_identical(listing(), before)

  # Replacing them is an explicit choice.
  suppressMessages(reflow_init(directory = proj, author = "A", depth = "minimal",
                               existing = TRUE, overwrite = TRUE, git = FALSE,
                               change_wd = FALSE, open = FALSE))
  expect_false(any(tools::md5sum(own) == before[own]))
  expect_true(any(grepl("theme.css", readLines(file.path(proj, "analysis", "_site.yml")),
                        fixed = TRUE)))
})

test_that("overwrite = TRUE re-themes a project made by an earlier call", {
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "retheme_proj")
  suppressMessages(reflow_init(directory = proj, author = "A", scheme = "brand",
                               depth = "minimal", git = FALSE,
                               change_wd = FALSE, open = FALSE))
  suppressMessages(reflow_init(directory = proj, author = "A", scheme = "clinical",
                               depth = "minimal", existing = TRUE, overwrite = TRUE,
                               git = FALSE, change_wd = FALSE, open = FALSE))
  css <- readLines(file.path(proj, "analysis", "theme.css"))
  expect_true(any(grepl(paste0("--cttir-topic: ", reflow_scheme("clinical")$accent, ";"),
                        css, fixed = TRUE)))
  icon <- system.file("icons", "icon_clinical.svg", package = "reflowR")
  expect_identical(unname(tools::md5sum(file.path(proj, "assets", "icon.svg"))),
                   unname(tools::md5sum(icon)))
  expect_error(
    reflow_init(directory = proj, author = "A", existing = TRUE, overwrite = NA,
                git = FALSE, change_wd = FALSE, open = FALSE),
    "overwrite"
  )
})

test_that("overwrite = TRUE changes nothing when a file cannot be replaced", {
  skip_on_os("windows")
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "locked_proj")
  suppressMessages(reflow_init(directory = proj, author = "A", scheme = "brand",
                               depth = "minimal", git = FALSE,
                               change_wd = FALSE, open = FALSE))
  # The README is written last, so without the check everything else would
  # already be re-themed when writing it fails.
  locked <- file.path(proj, "README.md")
  Sys.chmod(locked, "0444")
  withr::defer(Sys.chmod(locked, "0644"))
  skip_if(file.access(locked, 2) == 0, "File modes are not enforced for this user")
  listing <- function() {
    tools::md5sum(list.files(proj, recursive = TRUE, all.files = TRUE,
                             full.names = TRUE))
  }
  before <- listing()
  err <- expect_error(
    reflow_init(directory = proj, author = "A", scheme = "clinical",
                depth = "minimal", existing = TRUE, overwrite = TRUE,
                git = FALSE, change_wd = FALSE, open = FALSE),
    "nothing was changed"
  )
  expect_match(conditionMessage(err), "README.md", fixed = TRUE)
  expect_identical(listing(), before)
})

test_that("a missing file needs a writable folder above it", {
  skip_on_os("windows")
  tmp <- withr::local_tempdir()
  expect_true(reflow_init_writable(file.path(tmp, "new", "deeper", "file.txt")))
  locked <- file.path(tmp, "locked")
  dir.create(locked)
  Sys.chmod(locked, "0555")
  withr::defer(Sys.chmod(locked, "0755"))
  skip_if(file.access(locked, 2) == 0, "File modes are not enforced for this user")
  expect_false(reflow_init_writable(file.path(locked, "new", "file.txt")))
  expect_false(reflow_init_writable(tmp))
})

test_that("the replaced file list matches what reflow_init changes in a workflowr project", {
  tmp <- withr::local_tempdir()
  base <- file.path(tmp, "base")
  themed <- file.path(tmp, "themed")
  workflowr::wflow_start(base, name = "x", git = FALSE, change_wd = FALSE)
  suppressMessages(reflow_init(directory = themed, name = "x", author = "A",
                               depth = "minimal", git = FALSE,
                               change_wd = FALSE, open = FALSE))
  common <- intersect(list.files(base, recursive = TRUE, all.files = TRUE),
                      list.files(themed, recursive = TRUE, all.files = TRUE))
  changed <- common[unname(tools::md5sum(file.path(base, common))) !=
                      unname(tools::md5sum(file.path(themed, common)))]
  expect_setequal(changed, reflow_init_replaced)
})

test_that("the checked file list matches what reflow_init adds", {
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "listed_proj")
  reflow_init(directory = proj, author = "A", depth = "extended",
              git = FALSE, change_wd = FALSE, open = FALSE)
  added <- reflow_init_added(reflow_preset("extended"))
  expect_true(all(file.exists(file.path(proj, added))))
  rmds <- list.files(file.path(proj, "analysis"), pattern = "^\\d+.*\\.Rmd$")
  expect_setequal(file.path("analysis", rmds), grep("^analysis/\\d", added, value = TRUE))
})

test_that("_workflowr.yml records the installed workflowr version", {
  installed <- utils::packageVersion
  local_mocked_bindings(
    packageVersion = function(pkg, ...) {
      if (identical(pkg, "workflowr")) package_version("99.1.2") else installed(pkg, ...)
    },
    .package = "utils"
  )
  tmp <- withr::local_tempdir()
  proj <- file.path(tmp, "version_proj")
  reflow_init(directory = proj, author = "A", depth = "minimal",
              git = FALSE, change_wd = FALSE, open = FALSE)
  wf <- readLines(file.path(proj, "_workflowr.yml"), warn = FALSE)
  expect_true("workflowr_version: 99.1.2" %in% wf)
})
