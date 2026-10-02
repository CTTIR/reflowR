#' Execute or verify an immutable artifact graph generation
#'
#' A new generation can reference unchanged nodes in a completed prior
#' generation without copying their files. A node is reused only when its
#' complete resolved definition and producer lineage match. Changed inputs
#' never overwrite a prior generation. Resume requires the same plan, inputs
#' and runtime and rechecks accepted artifacts before dispatching any writer.
#'
#' Execution is serial and trusted, not sandboxed or resource supervised.
#' Stale locks require external inspection; they are never stolen. Failed
#' child attempts retain the explicit reconciliation contract of
#' [reflow_artifact_resume()]. Atomic writes provide rename visibility, not
#' power-loss durability. Keep prior generations available while referenced.
#' @param plan A [reflow_artifact_plan()].
#' @param directory Fresh generation directory, or existing one for resume.
#' @param previous Optional completed generation to verify for in-place reuse.
#' @param reconciliations Named list keyed by failed stage identifiers. Each
#'   element has exactly `reconciled_attempt` and `reconciliation`, as in
#'   [reflow_artifact_resume()]. No process absence is inferred automatically.
#' @return A graph descriptor containing authenticated node descriptors.
#' @export
reflow_artifact_plan_run <- function(plan, directory, previous = NULL) {
  rfg_execute(plan, directory, previous, fresh = TRUE, list())
}

#' @rdname reflow_artifact_plan_run
#' @export
reflow_artifact_plan_resume <- function(plan, directory,
                                        reconciliations = list()) {
  rfg_execute(plan, directory, NULL, fresh = FALSE, reconciliations)
}

rfg_lock <- function(directory) {
  path <- file.path(directory, ".lock")
  rfa_no_links(path)
  if (!dir.create(path, showWarnings = FALSE)) {
    stop("Graph locked; reconcile owner before release.")
  }
  tryCatch(rf_write(
    list(pid = Sys.getpid(), host = Sys.info()[["nodename"]]),
    file.path(path, "owner.rds")
  ), error = function(e) {
    unlink(path, recursive = TRUE)
    stop(e)
  })
  path
}

rfg_read <- function(path) {
  rfa_no_links(path)
  rfa_read(path)
}

rfg_records <- function(directory, definition, complete = FALSE) {
  rfg_saved_definition(definition)
  previous <- rfg_read(file.path(directory, "previous.rds"))
  if (!identical(previous, definition$previous)) stop("Previous binding differs.")
  journal <- file.path(directory, "nodes.rds")
  rfa_no_links(journal)
  nodes <- rfg_read(journal)
  ids <- names(definition$plan$stages)
  entries <- list.files(directory, all.files = TRUE, no.. = TRUE)
  allowed <- c(
    ".lock", "definition.rds", "previous.rds", "nodes.rds",
    "READY.rds", ids
  )
  if (any(!entries %in% allowed)) stop("Unexpected graph inventory.")
  for (id in intersect(ids, entries)) {
    path <- file.path(directory, id)
    rfa_no_links(path)
    if (!rfa_directory(path)) stop("Stage path must be a directory.")
  }
  if (!is.list(nodes) || is.object(nodes) ||
    length(nodes) && (is.null(names(nodes)) || anyDuplicated(names(nodes)) ||
      !all(names(nodes) %in% ids))) {
    stop("Invalid node journal.")
  }
  if (!identical(names(nodes), utils::head(ids, length(nodes))) && length(nodes)) {
    stop("Node journal is not an execution prefix.")
  }
  pending <- setdiff(ids, names(nodes))
  for (id in intersect(ids, entries)) {
    local <- rfa_canonical(file.path(directory, id))
    if (id %in% names(nodes)) {
      if (!identical(local, nodes[[id]]$descriptor$directory)) {
        stop("Unexpected local shadow of a reused node.")
      }
    } else if (!identical(id, pending[1L])) {
      stop("Unexpected directory for an undispatched stage.")
    }
  }
  for (id in names(nodes)) {
    n <- nodes[[id]]
    if (!is.list(n) || !identical(
      names(n),
      c("descriptor", "definition", "dependencies")
    )) {
      stop("Malformed node record.")
    }
    d <- n$descriptor
    rfa_no_links(d$directory)
    saved <- rfg_read(file.path(d$directory, "definition.rds"))
    if (!identical(saved, n$definition) ||
      !identical(saved$spec, rfg_resolve(definition$plan$stages[[id]], nodes))) {
      stop("Node definition differs.")
    }
    verified <- rfa_descriptor(
      saved$spec, d$directory,
      rfg_read(file.path(d$directory, "READY.rds")), saved
    )
    if (!identical(d, verified)) stop("Node descriptor differs.")
    expected <- rfg_dependencies(definition$plan, id, nodes)
    if (!identical(n$dependencies, expected)) stop("Producer lineage differs.")
  }
  ready <- file.path(directory, "READY.rds")
  rfa_no_links(ready)
  if (complete || file.exists(ready)) {
    expected <- list(
      schema = "reflow_artifact_graph_ready_1",
      definition_hash = rf_hash(definition), nodes = nodes
    )
    if (!identical(names(nodes), ids) ||
      !identical(rfg_read(ready), expected)) {
      stop("Graph READY differs.")
    }
  }
  nodes
}

rfg_dependencies <- function(plan, id, nodes) {
  ids <- plan$dependencies[[id]]
  if (!length(ids)) {
    return(list())
  }
  if (!all(ids %in% names(nodes))) stop("Producer has not completed.")
  lapply(nodes[ids], function(n) {
    list(
      definition_hash = rf_hash(n$definition),
      descriptor = n$descriptor, dependency_hash = rf_hash(n$dependencies)
    )
  })
}

rfg_resolve <- function(stage, nodes) {
  spec <- stage$declaration
  spec$args <- rfg_walk(spec$args, function(x) {
    d <- nodes[[x$stage]]$descriptor
    if (is.null(d)) stop("Unavailable producer.")
    path <- file.path(d$bundle, d$inventory$path[match(
      x$artifact,
      d$inventory$name
    )])
    rfa_no_links(path)
    reflow_imaging_input(path, read = x$read)
  })
  do.call(reflow_artifact_spec, rfg_spec_args(spec))
}

rfg_execute <- function(plan, directory, previous, fresh, reconciliations) {
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
  nodes <- rfg_records(directory, current)
  # Validate all completed inputs/runtime before any new writer is permitted.
  for (id in names(nodes)) {
    spec <- rfg_resolve(plan$stages[[id]], nodes)
    if (!identical(rfa_signature(spec), nodes[[id]]$definition)) {
      stop("Completed node inputs or runtime changed.")
    }
    reflow_artifact_resume(spec, nodes[[id]]$descriptor$directory)
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
    spec <- rfg_resolve(plan$stages[[id]], nodes)
    signature <- rfa_signature(spec)
    dependencies <- rfg_dependencies(plan, id, nodes)
    node_path <- file.path(directory, id)
    rfa_no_links(node_path)
    reuse <- !is.null(old[[id]]) &&
      identical(signature, old[[id]]$definition) &&
      identical(dependencies, old[[id]]$dependencies)
    if (reuse && !file.exists(node_path)) {
      descriptor <- reflow_artifact_resume(spec, old[[id]]$descriptor$directory)
    } else if (file.exists(node_path)) {
      retry <- reconciliations[[id]]
      descriptor <- do.call(
        reflow_artifact_resume,
        c(list(spec = spec, directory = node_path), retry)
      )
    } else {
      descriptor <- reflow_artifact_run(spec, node_path)
    }
    nodes[[id]] <- list(
      descriptor = descriptor, definition = signature,
      dependencies = dependencies
    )
    rf_write(nodes, file.path(directory, "nodes.rds"))
  }
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

# Validate historical shape without requiring historical inputs to be current.
rfg_saved_definition <- function(x) {
  if (!is.list(x) || !identical(
    names(x),
    c("schema", "plan", "signatures", "previous")
  ) ||
    !identical(x$schema, "reflow_artifact_graph_definition_1") ||
    !identical(class(x$plan), "reflow_artifact_plan") ||
    !identical(x$plan, do.call(
      reflow_artifact_plan,
      unname(x$plan$stages)
    )) ||
    !identical(names(x$signatures), names(x$plan$stages))) {
    stop("Malformed historical graph definition.")
  }
  for (id in names(x$signatures)) {
    signature <- x$signatures[[id]]
    spec <- x$plan$stages[[id]]$declaration
    spec$args <- rfg_walk(spec$args, function(x) "deferred-artifact")
    if (!is.list(signature) || !identical(
      names(signature),
      c("schema", "spec", "base", "resources", "tracked", "runtime")
    ) ||
      !identical(signature$schema, "reflow_artifact_definition_1") ||
      !identical(signature$spec, spec)) {
      stop("Malformed historical stage definition.")
    }
  }
  if (!is.null(x$previous)) {
    y <- x$previous
    if (!is.list(y) || !identical(names(y), c("directory", "ready_hash"))) {
      stop("Malformed previous binding.")
    }
    rfg_text(y$directory)
    rfg_text(y$ready_hash)
    if (!grepl("^[a-f0-9]{64}$", y$ready_hash)) stop("Invalid prior hash.")
  }
  invisible(TRUE)
}

# Follow only explicit generation links, retaining all acquired locks on success.
rfg_history <- function(directory, expected = NULL, forbidden, acquire = TRUE) {
  locks <- character()
  success <- FALSE
  on.exit(if (!success) unlink(locks, recursive = TRUE), add = TRUE)
  seen <- character()
  first <- NULL
  binding <- NULL
  repeat {
    rfa_no_links(directory)
    directory <- rfa_canonical(directory)
    if (directory %in% seen || rfa_within(directory, forbidden) ||
      rfa_within(forbidden, directory)) {
      stop("Cyclic or overlapping history.")
    }
    seen <- c(seen, directory)
    if (acquire) locks <- c(locks, rfg_lock(directory))
    ready <- file.path(directory, "READY.rds")
    rfa_no_links(ready)
    if (!rfa_regular(ready)) stop("Prior READY must be a regular file.")
    hash <- rf_file_hash(ready)
    if (!is.null(expected) && !identical(hash, expected)) {
      stop("Prior READY changed.")
    }
    definition <- rfg_read(file.path(directory, "definition.rds"))
    nodes <- rfg_records(directory, definition, complete = TRUE)
    if (is.null(first)) {
      first <- nodes
      binding <- list(directory = directory, ready_hash = hash)
    }
    previous <- definition$previous
    if (is.null(previous)) break
    directory <- previous$directory
    expected <- previous$ready_hash
  }
  success <- TRUE
  list(nodes = first, binding = binding, locks = locks)
}
