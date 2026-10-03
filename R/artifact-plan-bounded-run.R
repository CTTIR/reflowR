# Private bounded graph scheduler; synchronous scheduler remains unchanged.
rfbg_execute <- function(plan, directory, previous, fresh, reconciliations,
                         supervisor_directory, resources, runtime_files, cancel) {
  control <- rfbg_preflight(plan, directory, supervisor_directory, resources,
                            runtime_files, cancel, fresh)
  rfg_text(directory)
  rfa_no_links(directory)
  if (!rfa_directory(dirname(directory))) stop("Destination parent must exist.")
  directory <- file.path(rfa_canonical(dirname(directory)), basename(directory))
  if (!is.list(reconciliations) || is.object(reconciliations) ||
    length(reconciliations) && (is.null(names(reconciliations)) ||
      anyDuplicated(names(reconciliations)) ||
      !all(names(reconciliations) %in% names(plan$stages)))) {
    stop("Invalid stage reconciliations.")
  }
  for (x in reconciliations) {
    if (!is.list(x) || !identical(
      sort(names(x)),
      c("reconciled_attempt", "reconciliation")
    )) {
      stop("Invalid reconciliation fields.")
    }
  }
  if (!fresh) {
    if (!rfa_directory(directory)) stop("Generation directory missing.")
    lock <- rfg_lock(directory)
    on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  }
  # Validate the complete declaration before creating any generation.
  current <- rfg_definition(plan)
  for (stage in plan$stages) {
    args <- rfg_walk(stage$declaration$args, function(x) "deferred-artifact")
    rf_walk(args, function(x) {
      input <- rfa_canonical(x$path)
      if (rfa_within(directory, input) || rfa_within(input, directory)) {
        stop("Generation overlaps tracked input.")
      }
      x
    })
  }
  old <- list()
  previous_binding <- NULL
  if (!is.null(previous)) {
    rfg_text(previous)
    rfa_no_links(previous)
    previous <- rfa_canonical(previous)
    rfbg_disjoint(control$directory, previous)
    if (rfa_within(directory, previous) || rfa_within(previous, directory)) {
      stop("Generations overlap.")
    }
    history <- rfg_history(previous, forbidden = directory)
    on.exit(unlink(history$locks, recursive = TRUE), add = TRUE)
    old <- history$nodes
    previous_binding <- history$binding
  }
  if (fresh) {
    if (file.exists(directory) || !dir.create(directory, showWarnings = FALSE)) {
      stop("Fresh generation required.")
    }
  } else if (!rfa_directory(directory)) {
    stop("Generation directory missing.")
  }
  if (fresh) {
    lock <- rfg_lock(directory)
    on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  }
  definition_path <- file.path(directory, "definition.rds")
  if (!fresh) previous_binding <- rfg_read(file.path(directory, "previous.rds"))
  current["previous"] <- list(previous_binding)
  if (fresh) {
    rf_write(current, definition_path)
    rf_write(previous_binding, file.path(directory, "previous.rds"))
    rf_write(list(), file.path(directory, "nodes.rds"))
  } else if (!identical(rfg_read(definition_path), current)) {
    stop("Graph definition, inputs or runtime changed; use a new generation.")
  }
  control$definition$graph_definition <- current
  rfbg_open(control, fresh)
  control_lock <- rfg_lock(control$directory)
  on.exit(unlink(control_lock, recursive = TRUE), add = TRUE)
  rfbg_control(control)
  nodes <- rfg_records(directory, current)
  rfbg_alignment(control, nodes)
  # Validate all completed inputs/runtime before any new writer is permitted.
  for (id in names(nodes)) {
    spec <- rfg_resolve(plan$stages[[id]], nodes)
    if (!identical(rfa_signature(spec), nodes[[id]]$definition)) {
      stop("Completed node inputs or runtime changed.")
    }
    rfb_ready_only(spec, nodes[[id]]$descriptor$directory, nodes[[id]]$definition)
    rfbg_completed(control, id, nodes[[id]], directory)
  }
  # An interrupted generation must retain its prior reuse binding.
  if (!fresh) {
    if (!is.null(previous_binding)) {
      history <- rfg_history(
        previous_binding$directory,
        previous_binding$ready_hash, directory
      )
      on.exit(unlink(history$locks, recursive = TRUE), add = TRUE)
      old <- history$nodes
    }
  }
  if (file.exists(file.path(directory, "READY.rds"))) {
    return(invisible(list(
      schema = "reflow_artifact_graph_descriptor_1",
      directory = directory, nodes = nodes
    )))
  }
  for (id in setdiff(names(plan$stages), names(nodes))) {
    rfbg_cancel(cancel)
    spec <- rfg_resolve(plan$stages[[id]], nodes)
    signature <- rfa_signature(spec)
    dependencies <- rfg_dependencies(plan, id, nodes)
    node_path <- file.path(directory, id)
    rfa_no_links(node_path)
    reuse <- !is.null(old[[id]]) &&
      identical(signature, old[[id]]$definition) &&
      identical(dependencies, old[[id]]$dependencies)
    if (reuse && !file.exists(node_path)) {
      descriptor <- rfb_ready_only(spec, old[[id]]$descriptor$directory, signature)
    } else {
      descriptor <- rfbg_dispatch(control, id, spec, node_path, signature,
                                  dependencies, reconciliations[[id]], cancel)
    }
    nodes[[id]] <- list(
      descriptor = descriptor, definition = signature,
      dependencies = dependencies
    )
    rf_write(nodes, file.path(directory, "nodes.rds"))
    rfbg_completed(control, id, nodes[[id]], directory)
  }
  rfbg_control(control)
  final <- rfg_definition(plan)
  final["previous"] <- list(previous_binding)
  if (!identical(final, current)) stop("Graph changed during run.")
  rfg_records(directory, current)
  if (!is.null(previous_binding)) {
    rfg_history(previous_binding$directory, previous_binding$ready_hash,
      directory,
      acquire = FALSE
    )
  }
  ready <- list(
    schema = "reflow_artifact_graph_ready_1",
    definition_hash = rf_hash(current), nodes = nodes
  )
  rf_write(ready, file.path(directory, "READY.rds"))
  invisible(list(
    schema = "reflow_artifact_graph_descriptor_1",
    directory = directory, nodes = nodes
  ))
}
