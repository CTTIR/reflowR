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
