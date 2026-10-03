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
\item{resources}{Exact list: timeout_seconds (1--60), address_space_bytes,
 nice (absolute 0--19), threads (1--2), temp_directory, cache_directory,
 rscript, prlimit, nice_command. Paths are canonical and link-free.}
\item{runtime_files}{Named SHA-256 vector including installed backend scripts,
 every file of the backend and declared writer Depends/Imports/LinkingTo
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
