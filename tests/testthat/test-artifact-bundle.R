artifact_example <- function(text = "literal", ...) {
  reflow_artifact_spec(
    "base", "writeLines", args = list(text = text),
    outputs = list(con = list(path = "result.txt", type = "file")),
    inventory = c(result = "result.txt"),
    ...
  )
}

test_that(
  "declared exported writer produces authenticated bytes and immutable reuse",
  {
    root <- tempfile("bundle space ")
    dir.create(root)
    on.exit(
      unlink(root, recursive = TRUE),
      add = TRUE
    )
    run <- file.path(root, "run")
    spec <- artifact_example()
    answer <- reflow_artifact_run(spec, run)
    expect_equal(
      readLines(file.path(answer$bundle, "result.txt")),
      "literal"
    )
    expect_identical(answer$inventory$bytes, 8)
    expect_identical(
      answer$inventory$sha256, digest::digest(
        charToRaw("literal\n"),
        algo = "sha256", serialize = FALSE
      )
    )
    before <- readBin(
      file.path(run, "READY.rds"),
      "raw", n = 1e+06
    )
    expect_identical(
      reflow_artifact_resume(spec, run),
      answer
    )
    expect_length(
      list.files(run, pattern = "^attempt-"),
      1
    )
    expect_error(
      reflow_artifact_run(spec, run),
      "Fresh"
    )
    expect_error(
      reflow_artifact_resume(
        artifact_example("changed"),
        run
      ),
      "changed"
    )
    expect_identical(
      readBin(
        file.path(run, "READY.rds"),
        "raw", n = 1e+06
      ),
      before
    )
    writeLines("changed", file.path(answer$bundle, "result.txt"))
    expect_error(
      reflow_artifact_resume(spec, run),
      "Corrupt"
    )
    expect_identical(
      readBin(
        file.path(run, "READY.rds"),
        "raw", n = 1e+06
      ),
      before
    )
  }
)

test_that(
  "declarations refuse ambiguous ownership and unsafe paths", {
    bad <- c(
      "/absolute", "../parent", "a/../b", "a//b", "a/", "a\\b", "a:b",
      "."
    )
    for (path in bad) {
      expect_error(
        reflow_artifact_spec(
          "base", "writeLines", outputs = list(con = list(path = path,
                                                          type = "file")),
          inventory = c(x = path)
        )
      )
    }
    expect_error(
      reflow_artifact_spec(
        "base", "writeLines", args = list(con = "old"),
        outputs = list(con = list(path = "a", type = "file")),
        inventory = c(x = "a")
      ),
      "overlap"
    )
    expect_error(
      reflow_artifact_spec(
        "base", "writeLines", outputs = list(con = list(path = "a",
                                                        type = "directory")),
        inventory = c(x = "a/X", y = "a/x")
      ),
      "case-colliding"
    )
    expect_error(
      reflow_artifact_spec(
        "base", "writeLines", outputs = list(con = list(path = "a",
                                                        type = "file")),
        inventory = c(x = "b")
      ),
      "owner"
    )
    expect_error(
      artifact_example(allow_empty = "unknown"),
      "allow_empty"
    )
    expect_error(
      reflow_artifact_spec(
        "base", "writeLines",
        args = list(text = reflow_imaging_ref("previous")),
        outputs = list(con = list(path = "a", type = "file")),
        inventory = c(x = "a")
      ),
      "references"
    )
  }
)

test_that(
  "empty files and runtime mutation are explicit", {
    root <- tempfile()
    dir.create(root)
    on.exit(
      unlink(root, recursive = TRUE),
      add = TRUE
    )
    failed <- file.path(root, "failed")
    expect_error(
      reflow_artifact_run(
        artifact_example(character()),
        failed
      ),
      "empty"
    )
    expect_false(file.exists(file.path(failed, "READY.rds")))
    expect_error(
      reflow_artifact_resume(
        artifact_example(character()),
        failed
      ),
      "reconciliation"
    )
    state <- readRDS(file.path(failed, "state.rds"))
    expect_error(
      reflow_artifact_resume(
        artifact_example(character()),
        failed, reconciled_attempt = state$attempt,
        reconciliation = "Writer and children stopped"
      ),
      "empty"
    )
    expect_length(
      list.files(failed, pattern = "^attempt-"),
      2
    )
    ok <- reflow_artifact_run(
      artifact_example(character(), allow_empty = "result"),
      file.path(root, "ok")
    )
    expect_identical(ok$inventory$bytes, 0)
    runtime <- file.path(root, "engine")
    writeLines("v1", runtime)
    spec <- artifact_example(runtime_files = runtime)
    reflow_artifact_run(spec, file.path(root, "runtime"))
    writeLines("v2", runtime)
    expect_error(
      reflow_artifact_resume(spec, file.path(root, "runtime")),
      "changed"
    )
  }
)

test_that(
  "strict inventories and receipt links fail without altering accepted state",
  {
    root <- tempfile()
    dir.create(root)
    on.exit(
      unlink(root, recursive = TRUE),
      add = TRUE
    )
    run <- file.path(root, "run")
    spec <- artifact_example()
    answer <- reflow_artifact_run(spec, run)
    dir.create(file.path(answer$bundle, "empty"))
    expect_error(
      reflow_artifact_resume(spec, run),
      "inventory"
    )
    unlink(
      file.path(answer$bundle, "empty"),
      recursive = TRUE
    )
    writeLines("extra", file.path(answer$bundle, "extra"))
    expect_error(
      reflow_artifact_resume(spec, run),
      "inventory"
    )
    unlink(file.path(answer$bundle, "extra"))
    ready <- readRDS(file.path(run, "READY.rds"))
    malformed <- ready
    malformed$attempt <- "../escape"
    saveRDS(malformed, file.path(run, "READY.rds"))
    expect_error(
      reflow_artifact_resume(spec, run),
      "Malformed"
    )
    saveRDS(ready, file.path(run, "READY.rds"))
    dir.create(file.path(run, ".lock"))
    expect_error(
      reflow_artifact_resume(spec, run),
      "locked"
    )
    expect_true(dir.exists(file.path(run, ".lock")))
  }
)

test_that(
  "installed exported writer failures, RNG and resources are authenticated",
  {
    root <- tempfile()
    dir.create(root)
    on.exit(
      unlink(root, recursive = TRUE),
      add = TRUE
    )
    lib <- file.path(root, "library")
    dir.create(lib)
    fixture <- test_path("fixtures", "artifact-writer")
    log <- file.path(root, "install.log")
    code <- system2(
      file.path(
        R.home("bin"),
        "R"
      ),
      c(
        "CMD", "INSTALL", "--no-byte-compile", paste0("--library=",
                                                      shQuote(lib)),
        shQuote(fixture)
      ),
      stdout = log, stderr = log
    )
    expect_identical(code, 0L)
    old <- .libPaths()
    .libPaths(c(lib, old))
    on.exit(
      .libPaths(old),
      add = TRUE
    )
    on.exit(
      unloadNamespace("artifactfixture"),
      add = TRUE
    )
    make <- function(mode = "ordinary", input = NULL, seed = NULL) {
      args <- list(mode = mode)
      if (!is.null(input))
        args$input <- reflow_imaging_input(input)
      reflow_artifact_spec(
        "artifactfixture", "write_bundle", args,
        outputs = list(path = list(path = "files", type = "directory")),
        inventory = c(first = "files/first.txt"),
        seed = seed
      )
    }
    answer <- reflow_artifact_run(make(), file.path(root, "ordinary"))
    expect_identical(
      readLines(file.path(answer$bundle, "files", "first.txt")),
      "first"
    )
    expect_s3_class(answer, "reflow_artifact_descriptor")
    for (mode in c("partial", "interrupt", "extra")) {
      run <- file.path(root, mode)
      condition <- tryCatch(
        reflow_artifact_run(
          make(mode),
          run
        ),
        error = identity, interrupt = identity
      )
      expect_s3_class(condition, "condition")
      expect_false(file.exists(file.path(run, "READY.rds")))
      state <- readRDS(file.path(run, "state.rds"))
      expect_identical(state$status, "FAILED")
      expect_true(
        file.exists(file.path(run, state$attempt, "bundle", "files",
                              "first.txt"))
      )
      expect_error(
        reflow_artifact_resume(
          make(mode),
          run
        ),
        "reconciliation"
      )
    }
    set.seed(42)
    before <- .Random.seed
    expect_error(
      reflow_artifact_run(
        make("random"),
        file.path(root, "unseeded")
      ),
      "seed"
    )
    expect_identical(.Random.seed, before)
    first <- reflow_artifact_run(
      make("random", seed = 7),
      file.path(root, "seeded1")
    )
    second <- reflow_artifact_run(
      make("random", seed = 7),
      file.path(root, "seeded2")
    )
    expect_identical(first$inventory, second$inventory)
    expect_identical(.Random.seed, before)
    input <- file.path(root, "input")
    writeLines("original", input)
    expect_error(
      reflow_artifact_run(
        make("mutate", input),
        file.path(root, "mutation")
      ),
      "changed"
    )
    resource <- file.path(lib, "artifactfixture", "template.css")
    writeLines("old resource", resource)
    spec <- make()
    run <- file.path(root, "resource")
    reflow_artifact_run(spec, run)
    writeLines("changed resource", resource)
    expect_error(
      reflow_artifact_resume(spec, run),
      "changed"
    )
  }
)

test_that(
  "input destinations, links and named Unicode paths are guarded", {
    root <- tempfile()
    dir.create(root)
    on.exit(
      unlink(root, recursive = TRUE),
      add = TRUE
    )
    input <- file.path(root, "input")
    dir.create(input)
    spec <- reflow_artifact_spec(
      "base", "writeLines", args = list(text = reflow_imaging_input(input)),
      outputs = list(con = list(path = "a", type = "file")),
      inventory = c(x = "a")
    )
    expect_error(
      reflow_artifact_run(spec, file.path(input, "run")),
      "overlaps tracked"
    )
    spec <- reflow_artifact_spec(
      "base", "writeLines", args = list(text = "literal"),
      outputs = list(con = list(path = "space/α.txt", type = "file")),
      inventory = c(letter = "space/α.txt")
    )
    run <- file.path(root, "run")
    answer <- reflow_artifact_run(spec, run)
    expect_identical(
      readLines(file.path(answer$bundle, "space", "α.txt")),
      "literal"
    )
    target <- file.path(answer$bundle, "space", "α.txt")
    copied <- file.path(root, "copy")
    file.copy(target, copied)
    unlink(target)
    linked <- suppressWarnings(file.symlink(copied, target))
    if (linked) {
      expect_error(
        reflow_artifact_resume(spec, run),
        "Symbolic links"
      )
    } else {
      expect_false(file.exists(target))
    }
  }
)

test_that(
  "intermediate directory spelling and tracked empty directories are bound",
  {
    expect_error(
      reflow_artifact_spec(
        "base", "writeLines", outputs = list(con = list(path = "files",
                                                        type = "directory")),
        inventory = c(a = "files/A/one", b = "files/a/two")
      ),
      "ancestors"
    )
    root <- tempfile()
    dir.create(root)
    on.exit(
      unlink(root, recursive = TRUE),
      add = TRUE
    )
    input <- file.path(root, "input")
    dir.create(input)
    spec <- reflow_artifact_spec(
      "base", "writeLines", args = list(text = reflow_imaging_input(input)),
      outputs = list(con = list(path = "result", type = "file")),
      inventory = c(x = "result")
    )
    run <- file.path(root, "run")
    reflow_artifact_run(spec, run)
    dir.create(file.path(input, "newempty"))
    expect_error(
      reflow_artifact_resume(spec, run),
      "changed"
    )
  }
)


test_that("array declarations and extra fields fail before execution", {
  outputs <- matrix(list(list(path = "x", type = "file")), 1L, 1L)
  names(outputs) <- "con"
  expect_error(reflow_artifact_spec("base", "writeLines", outputs = outputs,
                                    inventory = c(x = "x")), "named list")
  malformed <- list(con = list(path = "x", type = "file", extra = TRUE))
  expect_error(
    reflow_artifact_spec("base", "writeLines", outputs = malformed,
                         inventory = c(x = "x")),
    "exactly"
  )
})

test_that("lost attempt state cannot start another writer", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  run <- file.path(root, "run")
  spec <- artifact_example(character())
  expect_error(reflow_artifact_run(spec, run), "empty")
  unlink(file.path(run, "state.rds"))
  expect_error(reflow_artifact_resume(spec, run), "Orphan")
  expect_length(list.files(run, pattern = "^attempt-"), 1L)
  expect_false(file.exists(file.path(run, "READY.rds")))
})

test_that("portable paths reject reserved or ambiguous components", {
  for (path in c("CON", "NUL.txt", "aux/x", "Lpt9.log", "x.", "x ",
                 "x?/a", "a|b", 'a"b', "a<b")) {
    outputs <- list(con = list(path = path, type = "file"))
    expect_error(
      reflow_artifact_spec("base", "writeLines", outputs = outputs,
                           inventory = c(x = path)),
      "Reserved"
    )
  }
})

test_that("ancestor checks normalize separators and root boundaries", {
  within <- reflowR:::rfa_within
  expect_true(within("C:\\Input Space\\new", "c:/input space/", TRUE))
  expect_true(within("C:/new", "c:/", TRUE))
  expect_false(within("C:/input-other", "C:/input", TRUE))
  expect_true(within("/new", "/", FALSE))
  expect_false(within("/A/new", "/a", FALSE))
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  ancestor <- normalizePath(dirname(root), winslash = "/")
  args <- list(text = reflow_imaging_input(ancestor))
  outputs <- list(con = list(path = "x", type = "file"))
  spec <- reflow_artifact_spec("base", "writeLines", args = args,
                               outputs = outputs, inventory = c(x = "x"))
  expect_error(reflow_artifact_run(spec, file.path(root, "new")), "overlaps")
  expect_false(file.exists(file.path(root, "new")))
})


test_that("file and directory type predicates are distinct", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  file <- file.path(root, "file")
  writeLines("value", file)
  expect_true(reflowR:::rfa_directory(root))
  expect_false(reflowR:::rfa_directory(file))
  expect_false(reflowR:::rfa_regular(root))
  expect_true(reflowR:::rfa_regular(file))
})

test_that("non-file attempt state is refused before receipt reading", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  spec <- artifact_example(character())
  run <- file.path(root, "run")
  expect_error(reflow_artifact_run(spec, run), "empty")
  state <- file.path(run, "state.rds")
  unlink(state)
  dir.create(state)
  expect_error(reflow_artifact_resume(spec, run), "regular file")
  expect_length(list.files(run, pattern = "^attempt-"), 1L)
  expect_false(file.exists(file.path(run, "READY.rds")))
})

test_that("incomplete and conflicting declarations are refused", {
  declare <- function(outputs, inventory) {
    reflow_artifact_spec("base", "writeLines", outputs = outputs,
                         inventory = inventory)
  }
  expect_error(declare(list(con = list(path = "x", type = "socket")),
                       c(x = "x")), "Invalid output")
  expect_error(declare(list(a = list(path = "x", type = "directory"),
                            b = list(path = "x/y", type = "file")),
                       c(x = "x/y")), "roots overlap")
  expect_error(declare(list(a = list(path = "x", type = "directory")),
                       c(a = "x/a", b = "x/a/b")), "Invalid file inventory")
  expect_error(declare(list(a = list(path = "x", type = "file"),
                            b = list(path = "y", type = "file")),
                       c(x = "x")), "own at least")
  expect_error(declare(list(list(path = "x", type = "file")),
                       c(x = "x")), "unique nonempty names")
  expect_error(artifact_example(runtime_files = NA_character_), "runtime_files")
  expect_error(artifact_example(runtime_files = tempdir()), "regular files")
})

test_that("tracked RDS values and relative destinations preserve literal output", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  input <- file.path(root, "input.rds")
  saveRDS(c("first", "second"), input)
  spec <- artifact_example(reflow_imaging_input(input, read = TRUE))
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  result <- reflow_artifact_run(spec, "relative-run")
  expect_identical(readLines(file.path(result$bundle, "result.txt")),
                   c("first", "second"))
  expect_identical(reflow_artifact_resume(spec, "relative-run"), result)
  expect_error(reflow_artifact_run(spec, "absent/run"), "parent must exist")
  expect_false(dir.exists("absent"))
})

test_that("altered declarations fail before writing a definition", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  spec <- artifact_example()
  spec$schema <- "unknown"
  expect_error(reflow_artifact_run(spec, file.path(root, "schema")), "schema")
  spec <- artifact_example()
  spec$unexpected <- "extra"
  expect_error(reflow_artifact_run(spec, file.path(root, "extra")))
  expect_false(file.exists(file.path(root, "extra", "READY.rds")))
})

test_that("failed-state corruption cannot authorize another attempt", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  spec <- artifact_example(character())
  run <- file.path(root, "run")
  expect_error(reflow_artifact_run(spec, run), "empty")
  state_path <- file.path(run, "state.rds")
  original <- readRDS(state_path)
  changed <- original
  changed$status <- "READY"
  saveRDS(changed, state_path)
  expect_error(reflow_artifact_resume(spec, run), "Malformed attempt state")
  changed <- original
  changed$extra <- TRUE
  saveRDS(changed, state_path)
  expect_error(reflow_artifact_resume(spec, run), "Malformed attempt fields")
  changed <- original
  changed$error <- "different failure"
  saveRDS(changed, state_path)
  expect_error(reflow_artifact_resume(spec, run), "differs from receipt")
  expect_length(list.files(run, pattern = "^attempt-"), 1L)
  expect_false(file.exists(file.path(run, "READY.rds")))
})

test_that("accepted attempt disagreement cannot mutate accepted bytes", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  spec <- artifact_example()
  run <- file.path(root, "run")
  result <- reflow_artifact_run(spec, run)
  ready <- readRDS(file.path(run, "READY.rds"))
  receipt <- file.path(run, ready$attempt, "receipt.rds")
  changed <- ready
  changed$inventory$bytes <- changed$inventory$bytes + 1
  saveRDS(changed, receipt)
  expect_error(reflow_artifact_resume(spec, run), "receipt differs")
  expect_identical(readRDS(file.path(run, "READY.rds")), ready)
  expect_identical(readLines(file.path(result$bundle, "result.txt")), "literal")
})

test_that("definition changes across initialization are rejected", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  original_signature <- reflowR:::rfa_signature
  calls <- 0L
  testthat::local_mocked_bindings(rfa_signature = function(spec) {
    calls <<- calls + 1L
    answer <- original_signature(spec)
    if (calls == 2L) answer$changed_resource <- "concurrent resource change"
    answer
  }, .package = "reflowR")
  run <- file.path(root, "run")
  expect_error(reflow_artifact_run(artifact_example(), run),
               "changed before initialization")
  expect_false(file.exists(file.path(run, "definition.rds")))
  expect_length(list.files(run, pattern = "^attempt-"), 0L)
})

test_that("a resource change during reuse leaves accepted evidence untouched", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  spec <- artifact_example()
  run <- file.path(root, "run")
  result <- reflow_artifact_run(spec, run)
  ready <- readRDS(file.path(run, "READY.rds"))
  original_signature <- reflowR:::rfa_signature
  calls <- 0L
  testthat::local_mocked_bindings(rfa_signature = function(spec) {
    calls <<- calls + 1L
    answer <- original_signature(spec)
    if (calls == 2L) answer$changed_resource <- "concurrent resource change"
    answer
  }, .package = "reflowR")
  expect_error(reflow_artifact_resume(spec, run), "changed during verification")
  expect_identical(readRDS(file.path(run, "READY.rds")), ready)
  expect_identical(readLines(file.path(result$bundle, "result.txt")), "literal")
})

test_that("failed final verification retracts the accepted pointer", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  original_descriptor <- reflowR:::rfa_descriptor
  calls <- 0L
  testthat::local_mocked_bindings(rfa_descriptor = function(...) {
    calls <<- calls + 1L
    if (calls == 2L) stop("injected final verification failure")
    original_descriptor(...)
  }, .package = "reflowR")
  run <- file.path(root, "run")
  expect_error(reflow_artifact_run(artifact_example(), run),
               "injected final verification failure")
  expect_false(file.exists(file.path(run, "READY.rds")))
  state <- readRDS(file.path(run, "state.rds"))
  expect_identical(state$status, "FAILED")
  expect_identical(readRDS(file.path(run, state$attempt, "receipt.rds")), state)
  expect_identical(readLines(file.path(run, state$attempt, "bundle", "result.txt")),
                   "literal")
})

test_that("reordered declaration fields are noncanonical", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  spec <- artifact_example()
  spec <- structure(rev(unclass(spec)), class = class(spec))
  expect_error(reflow_artifact_run(spec, file.path(root, "run")), "Noncanonical")
})

test_that("disappeared inputs and missing declared packages fail before attempts", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  input <- file.path(root, "source.txt")
  writeLines("source", input)
  spec <- artifact_example(reflow_imaging_input(input))
  unlink(input)
  run <- file.path(root, "missing-input")
  expect_error(reflow_artifact_run(spec, run))
  expect_false(dir.exists(run))
  spec <- artifact_example(packages = "reflowDeliberatelyAbsentFixturePackage")
  expect_error(reflow_artifact_run(spec, file.path(root, "package")),
               "Missing declared package")
  expect_false(dir.exists(file.path(root, "package")))
})

test_that("missing resource trees and actual FIFOs fail without reading streams", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  expect_error(reflowR:::rfa_tree(file.path(root, "absent")), "Missing directory")
  skip_on_os("windows")
  skip_if(!nzchar(Sys.which("mkfifo")), "mkfifo unavailable")
  fifo <- file.path(root, "stream")
  expect_identical(system2(Sys.which("mkfifo"), shQuote(fifo)), 0L)
  expect_error(reflowR:::rfa_tree(root), "regular files")
  spec <- artifact_example(reflow_imaging_input(fifo))
  run <- file.path(root, "run")
  expect_error(reflow_artifact_run(spec, run), "Tracked inputs")
  expect_false(dir.exists(run))
})

test_that("package closure changes stop initialization before any attempt", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  original <- reflowR:::rf_signature
  testthat::local_mocked_bindings(rf_signature = function(...) {
    answer <- original(...)
    answer$packages <- answer$packages[0]
    answer
  }, .package = "reflowR")
  run <- file.path(root, "run")
  expect_error(reflow_artifact_run(artifact_example(), run), "Package closure")
  expect_false(dir.exists(run))
})

test_that("filesystem creation failures never accept a partial attempt", {
  for (failure in c("attempt", "bundle", "parent")) {
    local({
      root <- tempfile()
      dir.create(root)
      on.exit(unlink(root, recursive = TRUE), add = TRUE)
      spec <- artifact_example()
      if (failure == "parent") {
        spec <- reflow_artifact_spec("base", "writeLines", args = list(text = "x"),
          outputs = list(con = list(path = "nested/result", type = "file")),
          inventory = c(result = "nested/result"))
      }
      original_create <- base::dir.create
      testthat::local_mocked_bindings(dir.create = function(path, ...) {
        refused <- switch(failure,
          attempt = grepl("^attempt-", basename(path)),
          bundle = identical(basename(path), "bundle"),
          parent = identical(basename(path), "nested"))
        if (refused) return(FALSE)
        original_create(path, ...)
      }, .package = "base")
      run <- file.path(root, "run")
      expect_error(reflow_artifact_run(spec, run), "Cannot create")
      expect_false(file.exists(file.path(run, "READY.rds")))
      attempts <- list.files(run, pattern = "^attempt-")
      expect_length(attempts, if (failure == "attempt") 0L else 1L)
      if (failure == "parent") {
        state <- readRDS(file.path(run, "state.rds"))
        expect_identical(state$status, "FAILED")
        expect_identical(readRDS(file.path(run, state$attempt, "receipt.rds")), state)
      }
    })
  }
})

test_that("a foreign READY inserted during the writer is never overwritten", {
  root <- tempfile()
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  run <- file.path(root, "run")
  foreign <- list(owner = "external actor", literal = "preserve these bytes")
  original <- reflowR:::rf_call
  testthat::local_mocked_bindings(rf_call = function(...) {
    answer <- original(...)
    saveRDS(foreign, file.path(run, "READY.rds"))
    answer
  }, .package = "reflowR")
  expect_error(reflow_artifact_run(artifact_example(), run),
               "Accepted receipt already exists")
  expect_identical(readRDS(file.path(run, "READY.rds")), foreign)
  expect_identical(readRDS(file.path(run, "state.rds"))$status, "FAILED")
  expect_length(list.files(run, pattern = "^attempt-"), 1L)
})
