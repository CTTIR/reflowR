\name{reflow_artifact_plan_run_bounded}
\alias{reflow_artifact_plan_run_bounded}
\alias{reflow_artifact_plan_resume_bounded}
\title{Run or resume a serial graph with per-node Linux guardians}
\usage{
reflow_artifact_plan_run_bounded(plan, directory, supervisor_directory,
  resources, runtime_files, previous = NULL, cancel = function() FALSE)
reflow_artifact_plan_resume_bounded(plan, directory, supervisor_directory,
  resources, runtime_files, cancel = function() FALSE, reconciliations = list())
}
\arguments{
\item{plan}{An artifact dependency plan.}
\item{directory}{Graph generation directory.}
\item{supervisor_directory}{Separate policy/control root with an existing parent.}
\item{resources}{Exact named list in canonical stage order; each element has the resource fields documented by \code{reflow_artifact_run_bounded}.}
\item{runtime_files}{Common named SHA-256 vector covering all writer closures and backend files.}
\item{previous}{Optional completed prior generation for verified in-place reuse.}
\item{cancel}{Function returning one nonmissing logical cancellation value.}
\item{reconciliations}{Explicit per-stage artifact retry reconciliations. Never clears guardian locks.}
}
\value{Verified graph descriptor.}
\description{Validate all stage resource policies before any writer, execute pending nodes serially and verify cache hits without new supervisors. Scientific definitions and synchronous graph APIs are unchanged.}
\details{Control policy and attempt journals are separate from graph inventory. Orphan READY adoption requires a matching successful guardian outcome and fresh locked verification. Missing journals, failed guardian outcomes and changed policies fail closed. Ordinary parent unwind releases its graph/control locks; stale locks after process death require external inspection. Guardian death is unqualified and requires external cleanup. Linux only, trusted descendants, per-process address space, requested threads, and explicit finite deadlines of 1--86400 seconds per node; no graph-wide deadline, parallel scheduler, selector or targets integration.}
\seealso{\code{\link{reflow_artifact_run_bounded}}, \code{\link{reflow_artifact_plan_run}}}
\examples{
# Needs Linux with processx, ps, prlimit and nice, and a package library
# without symbolic links. R CMD check links the packages into a temporary
# library when it limits them to the declared dependencies
# (_R_CHECK_SUGGESTS_ONLY_, which --as-cran turns on, or
# _R_CHECK_DEPENDS_ONLY_). A run starts a guardian and a worker process and
# pins every installed file they load, which takes a while.
\donttest{
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
  root <- tempfile("bounded-", tmpdir = normalizePath(tempdir(), mustWork = TRUE))
  work <- file.path(root, c("tmp", "cache"))
  for (path in c(root, work)) dir.create(path)
  plan <- reflow_artifact_plan(reflow_artifact_stage(
    "lines", "base", "writeLines",
    args = list(text = c("alpha", "beta")),
    outputs = list(con = list(path = "lines.txt", type = "file")),
    inventory = c(lines = "lines.txt")
  ))
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
    # One resource list per stage, in the canonical stage order
    graph <- reflow_artifact_plan_run_bounded(plan, file.path(root, "graph"),
                                              file.path(root, "supervisor"),
                                              list(lines = resources), pins)
    print(readLines(file.path(graph$nodes$lines$descriptor$bundle, "lines.txt")))
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
}
}
