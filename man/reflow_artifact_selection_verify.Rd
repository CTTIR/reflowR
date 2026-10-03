\name{reflow_artifact_selection_verify}
\alias{reflow_artifact_selection_verify}
\title{Verify a cached generation reference}
\usage{reflow_artifact_selection_verify(pointer, plan)}
\arguments{\item{pointer}{A compact artifact selection reference.}\item{plan}{The current artifact plan.}}
\description{Refuses incomplete or stale selections before using the existing graph resume verification. Verification acquires and releases locks but does not dispatch writers or mutate completed payloads or journals.}
\value{The verified graph descriptor for immediate consumption.}
