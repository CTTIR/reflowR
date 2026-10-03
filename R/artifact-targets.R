#' Always verify a compact artifact selection in targets
#'
#' Initialize the registry explicitly before running the pipeline. Each make
#' selects/verifies using the existing graph authority; the target stores only
#' a small reference. `targets::tar_read()` alone is unverified. Consumers must
#' call [reflow_artifact_selection_verify()] immediately before reading files.
#' Plan expressions are caller-owned code and must not embed raw payloads.
#' @param name A plain target name.
#' @param plan_command An unevaluated language object constructing the plan.
#' @param registry Canonical existing selector registry path.
#' @return One targets target object, with an always cue and RDS format.
#' @export
reflow_artifact_target <- function(name, plan_command, registry) {
  rfg_text(name)
  if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", name)) stop("Plain target name required.")
  if (!is.language(plan_command) || is.expression(plan_command)) {
    stop("One unevaluated plan language object required.")
  }
  registry <- rfs_path(registry)
  if (!requireNamespace("targets", quietly = TRUE)) stop("Optional targets capability unavailable.")
  command <- substitute(reflowR::reflow_artifact_select(PLAN, REGISTRY),
                         list(PLAN = plan_command, REGISTRY = registry))
  targets::tar_target_raw(name = name, command = command,
                          cue = targets::tar_cue(mode = "always"), format = "rds")
}
