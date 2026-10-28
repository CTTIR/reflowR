\name{reflow_artifact_run_bounded}
\alias{reflow_artifact_run_bounded}
\alias{reflow_artifact_resume_bounded}
\title{Run an artifact with an optional Linux guardian}
\usage{
reflow_artifact_run_bounded(spec, directory, supervisor_directory,
  resources, runtime_files, cancel = function() FALSE)
reflow_artifact_resume_bounded(spec, directory, supervisor_directory,
  resources, runtime_files, cancel = function() FALSE,
  reconciled_attempt = NULL, reconciliation = NULL)
}
\arguments{
\item{spec}{A declared artifact specification.}
\item{directory}{Artifact run directory.}
\item{supervisor_directory}{Fresh separate supervisor directory.}
\item{resources}{Exact list: timeout_seconds (finite, 1--86400), address_space_bytes,
 nice (absolute 0--19), threads (1--2), temp_directory, cache_directory,
 rscript, prlimit, nice_command. Paths are canonical and link-free.}
\item{runtime_files}{Named SHA-256 vector including installed backend scripts,
 every file of the backend and declared writer Depends/Imports
 closure and the three executables. Other runtime files
 must be explicitly declared; no complete ELF closure is inferred.}
\item{cancel}{Callback returning one nonmissing logical value. Errors cancel.}
\item{reconciled_attempt,reconciliation}{Explicit retry assertions as in
 \code{reflow_artifact_resume}; no automatic process reconciliation.}
}
\description{Additive optional Linux execution preserving the synchronous APIs.}
\details{Requires processx 3.9.0 or later, ps 1.9.3 or later, Linux /proc and
 declared nice/prlimit executables. Address-space bounds are per process,
 thread variables are requests, and this is not a sandbox. Trusted same-UID
 descendants must retain ownership tokens. Guardian death remains unresolved
 and requires external cleanup. Unknown cleanup retains locks; no stale lock
 is stolen. A successful worker is revalidated through a locked READY-only
 path which cannot invoke a writer. Deadlines exclude up to two additional
 five-second cleanup intervals. No graph or targets scheduling is performed.
 Parent OPENBLAS_NUM_THREADS, OMP_NUM_THREADS and MKL_NUM_THREADS must
 already equal the requested thread count. Child locale, RNG kind and full
 numerical environment must match the captured definition before dispatch.
 Parent in-process changes are not silently replayed; use an explicit artifact
 seed for stochastic writers. RNG state is not copied.}
\value{A verified artifact descriptor after successful guardian cleanup.}
\examples{
# Needs Linux with processx, ps, prlimit and nice, and a package library
# without symbolic links (R CMD check --as-cran links its packages). A run
# starts a guardian and a worker process and pins every installed file they
# load, which takes a while.
\donttest{
roots <- c("reflowR", "processx", "ps", "digest", "base")
usable <- identical(Sys.info()[["sysname"]], "Linux") &&
  requireNamespace("processx", quietly = TRUE) &&
  requireNamespace("ps", quietly = TRUE) && ps::ps_is_supported() &&
  nzchar(Sys.which("prlimit")) && nzchar(Sys.which("nice"))
if (usable) {
  # reflowR, processx, ps, digest, the writer package (base) and their imports
  packages <- unique(c(roots, unlist(tools::package_dependencies(
    roots, db = utils::installed.packages(), which = c("Depends", "Imports"),
    recursive = TRUE
  ))))
  usable <- !any(nzchar(Sys.readlink(find.package(packages))))
}
if (usable) {
  root <- tempfile("bounded-", tmpdir = normalizePath(tempdir(), mustWork = TRUE))
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

  # The parent thread settings must equal the requested thread count.
  threads <- c("OPENBLAS_NUM_THREADS", "OMP_NUM_THREADS", "MKL_NUM_THREADS")
  saved <- Sys.getenv(threads, unset = NA)
  Sys.setenv(OPENBLAS_NUM_THREADS = 1, OMP_NUM_THREADS = 1, MKL_NUM_THREADS = 1)
  bundle <- reflow_artifact_run_bounded(spec, file.path(root, "run"),
                                        file.path(root, "supervisor"),
                                        resources, pins)
  print(readLines(file.path(bundle$bundle, "lines.txt")))

  for (name in threads) {
    if (is.na(saved[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, as.list(saved[name]))
  }
  unlink(root, recursive = TRUE)
}
}
}
