\name{reflow_artifact_target}
\alias{reflow_artifact_target}
\title{Always verify a compact artifact selection in targets}
\usage{reflow_artifact_target(name, plan_command, registry)}
\arguments{\item{name}{A plain target name.}\item{plan_command}{One unevaluated language object constructing the plan.}\item{registry}{Canonical selector registry path, initialized before execution.}}
\description{Creates an optional targets RDS target with an always cue. Every make calls the selector; the stored value is only a compact reference. No graph payloads are copied. A targets cache read alone does not verify current files: consumers must call \code{\link{reflow_artifact_selection_verify}} immediately before use. Plan expressions must not embed raw payloads.}
\value{A targets target object.}
\examples{
if (requireNamespace("targets", quietly = TRUE)) {
  registry <- file.path(normalizePath(tempdir(), mustWork = TRUE), "registry")
  # The plan expression is stored unevaluated and runs at every make
  target <- reflow_artifact_target("selection", quote(my_plan()), registry)
  target$command$expr
}
}
