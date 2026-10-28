\name{reflow_artifact_select}
\alias{reflow_artifact_select}
\title{Select an immutable artifact graph generation}
\usage{reflow_artifact_select(plan, registry, initialize = FALSE, reconciliation = NULL)}
\arguments{
\item{plan}{A \code{reflow_artifact_plan}.}
\item{registry}{Canonical registry directory with an existing parent.}
\item{initialize}{Explicitly create an absent registry.}
\item{reconciliation}{NULL or exact list with pending_hash, generation, action, quiescent, reason and reconciliations. Actions: publish_complete, resume_incomplete or close_failed. Quiescent must be TRUE and is an operator assertion, not a detected fact.}}
\description{Serial trusted graph selection using existing graph definitions and historical verification. Unchanged nodes remain in prior generations without payload copies. Linux local filesystem qualification is required; cached references alone are unverified.}
\details{Registry locking precedes inspection. Every completed generation remains an integrity obligation, including failed unpublished siblings; verification releases all historical locks before graph operations. A pending transaction blocks ordinary selection. Explicit reconciliation never silently retries an absent or malformed graph. Event staging residues require external forensic recovery. Current-pointer replacement prevents closing that generation failed. Same-directory rename provides visibility, not power-loss durability. This API does not supervise resource use or complete workflow orchestration.}
\value{Compact reference with status REFERENCE_REQUIRES_VERIFICATION, or NULL for close_failed.}
\examples{
# The selector is qualified for local Linux file systems only
if (identical(Sys.info()[["sysname"]], "Linux")) {
  plan <- reflow_artifact_plan(reflow_artifact_stage(
    "lines", "base", "writeLines",
    args = list(text = c("alpha", "beta")),
    outputs = list(con = list(path = "lines.txt", type = "file")),
    inventory = c(lines = "lines.txt")
  ))
  root <- tempfile("selection-", tmpdir = normalizePath(tempdir(), mustWork = TRUE))
  dir.create(root)
  registry <- file.path(root, "registry")
  pointer <- reflow_artifact_select(plan, registry, initialize = TRUE)
  pointer$status

  # An unchanged plan selects the same generation again
  identical(reflow_artifact_select(plan, registry), pointer)
  unlink(root, recursive = TRUE)
}
}
