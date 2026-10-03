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
