# Run an artifact with an optional Linux guardian

Additive optional Linux execution preserving the synchronous APIs.

## Usage

``` r
reflow_artifact_run_bounded(spec, directory, supervisor_directory,
  resources, runtime_files, cancel = function() FALSE)
reflow_artifact_resume_bounded(spec, directory, supervisor_directory,
  resources, runtime_files, cancel = function() FALSE,
  reconciled_attempt = NULL, reconciliation = NULL)
```

## Arguments

- spec:

  A declared artifact specification.

- directory:

  Artifact run directory.

- supervisor_directory:

  Fresh separate supervisor directory.

- resources:

  Exact list: timeout_seconds (finite, 1–86400), address_space_bytes,
  nice (absolute 0–19), threads (1–2), temp_directory, cache_directory,
  rscript, prlimit, nice_command. Paths are canonical and link-free.

- runtime_files:

  Named SHA-256 vector including installed backend scripts, every file
  of the backend and declared writer Depends/Imports closure and the
  three executables. Other runtime files must be explicitly declared; no
  complete ELF closure is inferred.

- cancel:

  Callback returning one nonmissing logical value. Errors cancel.

- reconciled_attempt,reconciliation:

  Explicit retry assertions as in `reflow_artifact_resume`; no automatic
  process reconciliation.

## Details

Requires processx 3.9.0 or later, ps 1.9.3 or later, Linux /proc and
declared nice/prlimit executables. Address-space bounds are per process,
thread variables are requests, and this is not a sandbox. Trusted
same-UID descendants must retain ownership tokens. Guardian death
remains unresolved and requires external cleanup. Unknown cleanup
retains locks; no stale lock is stolen. A successful worker is
revalidated through a locked READY-only path which cannot invoke a
writer. The guardian must report readiness within `timeout_seconds`, but
no less than 5 and no more than 60 seconds; a guardian that misses this
has started no worker and is stopped. Deadlines exclude up to two
additional five-second cleanup intervals. No graph or targets scheduling
is performed. Parent OPENBLAS_NUM_THREADS, OMP_NUM_THREADS and
MKL_NUM_THREADS must already equal the requested thread count. Child
locale, RNG kind and full numerical environment must match the captured
definition before dispatch. Parent in-process changes are not silently
replayed; use an explicit artifact seed for stochastic writers. RNG
state is not copied.

## Value

A verified artifact descriptor after successful guardian cleanup.

## Examples

``` r
# Needs Linux with processx, ps, prlimit and nice, and a package library
# without symbolic links. R CMD check links the packages into a temporary
# library when it limits them to the declared dependencies
# (_R_CHECK_SUGGESTS_ONLY_, which --as-cran turns on, or
# _R_CHECK_DEPENDS_ONLY_). A run starts a guardian and a worker process and
# pins every installed file they load, which takes a while.
# \donttest{
roots <- c("reflowR", "processx", "ps", "digest", "base")
usable <- identical(Sys.info()[["sysname"]], "Linux") &&
  requireNamespace("processx", quietly = TRUE) &&
  requireNamespace("ps", quietly = TRUE) && ps::ps_is_supported() &&
  nzchar(Sys.which("prlimit")) && nzchar(Sys.which("nice"))
if (usable) {
  # The worker runs at priority 10 with 8 GiB of address space, which an
  # unprivileged session can only request when its own settings allow it.
  space <- grep("^Max address space", readLines("/proc/self/limits"), value = TRUE)
  space <- strsplit(trimws(sub("^Max address space", "", space)), " +")[[1]][1:2]
  usable <- ps::ps_get_nice(ps::ps_handle()) <= 10 && isTRUE(all(
    space == "unlimited" | suppressWarnings(as.numeric(space)) >= 8 * 1024^3
  ))
}
if (usable) {
  # reflowR, processx, ps, digest, the writer package (base) and their imports
  packages <- unique(c(roots, unlist(tools::package_dependencies(
    roots, db = utils::installed.packages(), which = c("Depends", "Imports"),
    recursive = TRUE
  ))))
  usable <- !any(nzchar(Sys.readlink(find.package(packages))))
}
if (usable) {
  root <- tempfile("bounded-", tmpdir = normalizePath(tempdir(), winslash = "/", mustWork = TRUE))
  work <- file.path(root, c("tmp", "cache"))
  for (path in c(root, work)) dir.create(path)
  spec <- reflow_artifact_spec(
    "base", "writeLines",
    args = list(text = c("alpha", "beta")),
    outputs = list(con = list(path = "lines.txt", type = "file")),
    inventory = c(lines = "lines.txt")
  )
  resources <- list(
    timeout_seconds = 60, address_space_bytes = 8 * 1024^3, nice = 10L,
    threads = 1L, temp_directory = work[1], cache_directory = work[2],
    rscript = normalizePath(file.path(R.home("bin"), "Rscript")),
    prlimit = normalizePath(Sys.which("prlimit")),
    nice_command = normalizePath(Sys.which("nice"))
  )
  # Pin the backend scripts, every installed file of those packages and the
  # three executables.
  files <- c(
    list.files(system.file("bounded", package = "reflowR"), full.names = TRUE),
    unlist(lapply(find.package(packages), list.files, recursive = TRUE,
                  full.names = TRUE, all.files = TRUE)),
    resources$rscript, resources$prlimit, resources$nice_command
  )
  files <- unique(normalizePath(files, winslash = "/"))
  pins <- vapply(files, digest::digest, character(1), algo = "sha256", file = TRUE)

  # The parent thread settings must equal the requested thread count; they
  # are restored afterwards, also when the run fails.
  threads <- c("OPENBLAS_NUM_THREADS", "OMP_NUM_THREADS", "MKL_NUM_THREADS")
  saved <- Sys.getenv(threads, unset = NA)
  Sys.setenv(OPENBLAS_NUM_THREADS = 1, OMP_NUM_THREADS = 1, MKL_NUM_THREADS = 1)
  tryCatch({
    bundle <- reflow_artifact_run_bounded(spec, file.path(root, "run"),
                                          file.path(root, "supervisor"),
                                          resources, pins)
    print(readLines(file.path(bundle$bundle, "lines.txt")))
  }, error = function(e) {
    # A loaded machine can leave the guardian too little time; report it.
    message("The bounded run did not complete here: ", conditionMessage(e))
  }, finally = {
    for (name in threads) {
      if (is.na(saved[[name]])) Sys.unsetenv(name)
      else do.call(Sys.setenv, as.list(saved[name]))
    }
    unlink(root, recursive = TRUE)
  })
}
#> [1] "alpha" "beta" 
# }
```
