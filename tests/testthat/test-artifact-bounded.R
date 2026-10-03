test_that("bounded resource declaration is exact and scalar", {
  valid <- list(
    timeout_seconds = 10, address_space_bytes = 8 * 1024^3,
    nice = 10L, threads = 2L, temp_directory = "/tmp",
    cache_directory = "/tmp", rscript = "/r",
    prlimit = "/p", nice_command = "/n"
  )
  expect_identical(rfb_resources(valid), valid)
  for (key in c("timeout_seconds", "address_space_bytes", "nice", "threads")) {
    for (value in list(NA_real_, Inf, 1i, matrix(1), structure(1, class = "unknown"))) {
      bad <- valid
      bad[[key]] <- value
      expect_error(rfb_resources(bad), "Invalid")
    }
  }
  expect_error(rfb_resources(c(valid, list(extra = TRUE))), "Exact")
  expect_error(rfb_resources(valid[-1]), "Exact")
  valid$timeout_seconds <- 86401
  expect_error(rfb_resources(valid), "Invalid")
})

test_that("READY-only revalidation cannot launch a writer on missing READY", {
  root <- tempfile(tmpdir = normalizePath(tempdir(), mustWork = TRUE))
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE))
  spec <- reflow_artifact_spec("base", "writeLines", list(text = "literal"),
    outputs = list(con = list(path = "x.txt", type = "file")),
    inventory = c(x = "x.txt")
  )
  run <- file.path(root, "run")
  expected <- rfa_signature(spec)
  descriptor <- reflow_artifact_run(spec, run)
  verified <- rfb_ready_only(spec, run, expected)
  # Compare path identity while preserving every other descriptor field exactly.
  for (field in c("directory", "bundle")) {
    verified[[field]] <- normalizePath(
      verified[[field]], winslash = "/", mustWork = TRUE
    )
    descriptor[[field]] <- normalizePath(
      descriptor[[field]], winslash = "/", mustWork = TRUE
    )
  }
  expect_identical(verified, descriptor)
  old <- list.files(run, recursive = TRUE, all.files = TRUE)
  unlink(file.path(run, "READY.rds"))
  expect_error(rfb_ready_only(spec, run, expected))
  expect_false(file.exists(file.path(run, "READY.rds")))
  expect_identical(
    list.files(run, pattern = "^attempt-"),
    unique(sub("/.*", "", grep("^attempt-", old, value = TRUE)))
  )
})

test_that("unsupported hosts refuse optional backend without changing synchronous API", {
  skip_if(identical(Sys.info()[["sysname"]], "Linux"))
  expect_error(
    reflow_artifact_run_bounded(NULL, NULL, NULL, NULL, NULL),
    "capability unavailable"
  )
})


test_that("the base writer is a concrete namespace path, not namespace metadata", {
  expect_identical(
    rfb_packages("base")[["base"]],
    file.path(R.home("library"), "base")
  )
})

test_that("READY-only validation refuses a newer internally consistent baseline", {
  root <- tempfile(tmpdir = normalizePath(tempdir(), mustWork = TRUE))
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE))
  before <- reflow_artifact_spec("base", "writeLines", list(text = "before"),
    outputs = list(con = list(path = "x.txt", type = "file")),
    inventory = c(x = "x.txt")
  )
  after <- reflow_artifact_spec("base", "writeLines", list(text = "after"),
    outputs = list(con = list(path = "x.txt", type = "file")),
    inventory = c(x = "x.txt")
  )
  expected <- rfa_signature(before)
  run <- file.path(root, "run")
  reflow_artifact_run(after, run)
  expect_error(rfb_ready_only(after, run, expected), "definition changed")
})

test_that("real captured definition is data-only while unknown classes refuse", {
  schema <- new.env(parent = baseenv())
  sys.source(system.file("bounded", "schema.R",
    package = "reflowR",
    mustWork = TRUE
  ), envir = schema)
  spec <- reflow_artifact_spec("base", "writeLines", list(text = "literal"),
    outputs = list(con = list(path = "x.txt", type = "file")),
    inventory = c(x = "x.txt")
  )
  expected <- rfa_signature(spec)
  expect_identical(class(expected$base$plan), "reflow_imaging_plan")
  expect_identical(class(expected$base$plan$stages[[1]]), "reflow_imaging_stage")
  request <- list(
    schema = 1L, spec = spec, expected_definition = expected,
    directory = "/anonymous", resume = FALSE, reconciled_attempt = NULL,
    reconciliation = NULL, libraries = .libPaths(),
    package_path = find.package("reflowR"), package_paths = c(base = find.package("base")),
    runtime_sha256 = character(), runtime_observation = list(
      locale = Sys.getlocale(),
      rng_kind = RNGkind()
    ), channel_script = "/anonymous/channel.R"
  )
  expect_true(schema$rb_request(request))
  request$expected_definition$base$plan <- structure(list(), class = "unknown_plan")
  expect_error(schema$rb_request(request), "Invalid bounded artifact request")
  expect_false(schema$rb_plain(function() NULL))
  expect_false(schema$rb_plain(new.env()))
})

test_that("parent thread mismatch refuses before any launch or environment mutation", {
  withr::local_envvar(c(
    OPENBLAS_NUM_THREADS = "2", OMP_NUM_THREADS = "2",
    MKL_NUM_THREADS = "2"
  ))
  expect_true(rfb_thread_preflight(2L))
  expect_error(rfb_thread_preflight(1L), "Parent numerical thread")
  Sys.unsetenv("OMP_NUM_THREADS")
  expect_error(rfb_thread_preflight(2L), "Parent numerical thread")
  expect_identical(Sys.getenv("OMP_NUM_THREADS", unset = NA_character_), NA_character_)
})


test_that("installed helpers resolve globals from their isolated environment", {
  skip_if_not_installed("codetools")
  helper <- new.env(parent = baseenv())
  directory <- system.file("bounded", package = "reflowR", mustWork = TRUE)
  for (name in c("executor.R", "channel.R", "client.R", "schema.R")) {
    sys.source(file.path(directory, name), envir = helper)
  }
  for (name in ls(helper)) {
    globals <- codetools::findGlobals(helper[[name]], merge = FALSE)$functions
    expect_true(all(vapply(globals, exists, logical(1),
      envir = helper,
      mode = "function", inherits = TRUE
    )), info = name)
  }
})

test_that("Linux helper validates regular paths in its isolated environment", {
  skip_if_not(identical(Sys.info()[["sysname"]], "Linux"))
  helper <- new.env(parent = baseenv())
  sys.source(system.file("bounded", "executor.R",
    package = "reflowR",
    mustWork = TRUE
  ), envir = helper)
  file <- tempfile(tmpdir = normalizePath(tempdir(), mustWork = TRUE))
  writeLines("literal", file)
  withr::defer(unlink(file))
  expect_identical(helper$rfx_path(file), file)
  expect_error(helper$rfx_path(dirname(file)), "Regular file required")
})
