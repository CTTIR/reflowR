#' Select an immutable artifact graph generation
#'
#' This trusted, serial selector reuses the graph definition and history
#' authorities. It copies no artifacts. Linux local filesystems only; rename
#' visibility is not power-loss durability. Locks are never stolen. Interrupted
#' event writes require external forensic reconciliation; valid incomplete
#' publications require an explicit quiescence assertion, not an inferred one.
#' Cached references never establish current artifact validity.
#' @param plan A [reflow_artifact_plan()].
#' @param registry Canonical registry directory with an existing parent.
#' @param initialize Explicitly create an absent registry.
#' @param reconciliation NULL or an exact named list with `pending_hash`,
#'   `generation`, `action`, `quiescent`, `reason`, and `reconciliations`.
#'   Actions are `publish_complete`, `resume_incomplete`, or `close_failed`.
#'   Only resume accepts child reconciliation records, passed to graph resume.
#' @return A compact reference with status REFERENCE_REQUIRES_VERIFICATION;
#'   close_failed returns NULL and never dispatches a writer.
#' @export
reflow_artifact_select <- function(plan, registry, initialize = FALSE,
                                   reconciliation = NULL) {
  rfs_execute(plan, registry, initialize, reconciliation, NULL)
}

#' Verify a cached generation reference before consumption
#'
#' Refuses incomplete or stale selections before graph resume. Existing graph
#' verification acquires locks but does not dispatch writers or mutate payloads
#' or journals in this complete-only path. A targets cache read alone does not
#' perform this check.
#' @param pointer Compact reference returned by [reflow_artifact_select()].
#' @param plan Current [reflow_artifact_plan()].
#' @return Existing verified graph descriptor for immediate consumption.
#' @export
reflow_artifact_selection_verify <- function(pointer, plan) { # nolint: object_length_linter.
  rfs_pointer_shape(pointer)
  rfs_execute(plan, pointer$registry, FALSE, NULL, pointer)
}

rfs_checkpoint <- function(point) invisible(point)

rfs_path <- function(registry) {
  rfg_text(registry)
  rfa_no_links(registry)
  if (!rfa_directory(dirname(registry))) stop("Registry parent missing.")
  canonical <- file.path(rfa_canonical(dirname(registry)), basename(registry))
  if (!identical(canonical, registry) || basename(registry) %in% c(".", "..")) {
    stop("Canonical registry path required.")
  }
  registry
}

rfs_keys <- function(x, keys) {
  if (!is.list(x) || is.object(x) || !is.null(dim(x)) ||
      !identical(names(x), keys)) stop("Malformed selector record.")
}

rfs_hash_text <- function(x) {
  rfg_text(x)
  if (!grepl("^[a-f0-9]{64}$", x)) stop("Invalid selector hash.")
}

rfs_pointer_shape <- function(x) {
  rfs_keys(x, c("schema", "registry", "event", "event_hash", "generation",
               "request_hash", "ready_hash", "status"))
  if (!identical(x$schema, "reflow_artifact_selection_1") ||
      !identical(x$status, "REFERENCE_REQUIRES_VERIFICATION") ||
      !is.integer(x$event) || length(x$event) != 1L || is.na(x$event) ||
      x$event < 1L) stop("Invalid selector pointer.")
  rfg_text(x$registry)
  rfg_text(x$generation)
  for (key in c("event_hash", "request_hash", "ready_hash")) {
    rfs_hash_text(x[[key]])
  }
}

rfs_pointer <- function(registry, complete, hash, pending) {
  list(schema = "reflow_artifact_selection_1", registry = registry,
       event = complete$sequence, event_hash = hash,
       generation = pending$generation, request_hash = rf_hash(pending$definition),
       ready_hash = complete$ready, status = "REFERENCE_REQUIRES_VERIFICATION")
}

# History returns held locks: release them here, before any graph API call.
rfs_history <- function(directory, ready = NULL, definition = NULL) {
  history <- rfg_history(directory, expected = ready,
                         forbidden = paste0(directory, "-selector-verification"))
  on.exit(unlink(history$locks, recursive = TRUE), add = TRUE)
  if (!is.null(definition)) {
    saved <- rfg_read(file.path(directory, "definition.rds"))
    saved$previous <- NULL
    if (!identical(saved, definition)) stop("Historical request binding differs.")
  }
  history$binding
}

rfs_current <- function(registry) {
  path <- file.path(registry, "current.rds")
  rfa_no_links(path)
  if (!file.exists(path)) return(NULL)
  x <- rfg_read(path)
  rfs_pointer_shape(x)
  x
}

rfs_event_path <- function(registry, sequence) {
  file.path(registry, "events", sprintf("%09d.rds", sequence))
}

rfs_read <- function(registry) {
  if (!rfa_directory(registry)) stop("Registry missing; initialization is explicit.")
  rfs_keys(rfg_read(file.path(registry, "registry.rds")),
           c("schema", "registry", "event_staging", "pointer_staging"))
  header <- list(schema = "reflow_artifact_registry_1", registry = registry,
                 event_staging = "event.tmp.rds", pointer_staging = "pointer.tmp.rds")
  if (!identical(rfg_read(file.path(registry, "registry.rds")), header)) {
    stop("Registry header differs.")
  }
  names_root <- list.files(registry, all.files = TRUE, no.. = TRUE)
  if (any(!names_root %in% c("registry.rds", "events", "generations",
                            "current.rds", "pointer.tmp.rds", "event.tmp.rds"))) {
    stop("Unexpected registry inventory.")
  }
  for (name in c("events", "generations")) {
    path <- file.path(registry, name)
    rfa_no_links(path)
    if (!rfa_directory(path)) stop("Registry directory missing.")
  }
  if ("event.tmp.rds" %in% names_root) {
    stop("Event staging residue requires external forensic reconciliation.")
  }
  files <- list.files(file.path(registry, "events"), all.files = TRUE, no.. = TRUE)
  if (length(files) >= .Machine$integer.max ||
      !identical(files, sprintf("%09d.rds", seq_along(files)))) {
    stop("Event journal has a gap or foreign entry.")
  }
  active <- NULL
  phase <- "IDLE"
  published <- NULL
  previous_hash <- NULL
  generations <- character()
  required <- character()
  completed_bindings <- list()
  complete <- pointer <- NULL
  pending_hash <- NULL
  for (i in seq_along(files)) {
    path <- rfs_event_path(registry, i)
    e <- rfg_read(path)
    rfs_keys(e, c("schema", "sequence", "previous", "type", "pending_hash",
                 "generation", "definition", "ready", "old_pointer", "reconciliation"))
    if (!identical(e$schema, "reflow_artifact_selection_event_1") ||
        !identical(e$sequence, as.integer(i)) || !identical(e$previous, previous_hash)) {
      stop("Event chain differs.")
    }
    hash <- rf_file_hash(path)
    if (identical(e$type, "PENDING")) {
      if (!identical(phase, "IDLE") || !is.null(e$pending_hash) ||
          !is.null(e$ready) || !is.null(e$reconciliation) ||
          !identical(e$old_pointer, published) ||
          !identical(e$generation, file.path(registry, "generations", sprintf("g%09d", i)))) {
        stop("Invalid PENDING transition.")
      }
      definition <- e$definition
      if (!is.list(definition) ||
          !identical(names(definition), c("schema", "plan", "signatures"))) {
        stop("Invalid pending definition.")
      }
      definition["previous"] <- list(NULL)
      rfg_saved_definition(definition)
      active <- e
      pending_hash <- hash
      generations <- c(generations, basename(e$generation))
      phase <- "PENDING"
    } else {
      if (is.null(active) || !identical(e$pending_hash, pending_hash) ||
          !identical(e$generation, active$generation) || !is.null(e$definition) ||
          !identical(e$old_pointer, active$old_pointer)) stop("Event pending binding differs.")
      if (!is.null(e$reconciliation)) rfs_reconciliation(e$reconciliation, active, pending_hash)
      if (identical(e$type, "COMPLETED")) {
        if (!identical(phase, "PENDING")) stop("Invalid COMPLETED transition.")
        rfs_hash_text(e$ready)
        complete <- e
        pointer <- rfs_pointer(registry, e, hash, active)
        required <- c(required, basename(e$generation))
        completed_bindings[[length(completed_bindings) + 1L]] <- list(
          directory = e$generation, ready_hash = e$ready,
          definition = active$definition)
        phase <- "COMPLETED"
      } else if (identical(e$type, "INTENT")) {
        if (!identical(phase, "COMPLETED") || !identical(e$ready, complete$ready)) {
          stop("Invalid INTENT transition.")
        }
        phase <- "INTENT"
      } else if (identical(e$type, "PUBLISHED")) {
        if (!identical(phase, "INTENT") || !identical(e$ready, complete$ready)) {
          stop("Invalid PUBLISHED transition.")
        }
        published <- pointer
        active <- complete <- pointer <- NULL
        pending_hash <- NULL
        phase <- "IDLE"
      } else if (identical(e$type, "FAILED")) {
        if (!phase %in% c("PENDING", "COMPLETED", "INTENT") ||
            !is.null(e$ready) || is.null(e$reconciliation) ||
            !identical(e$reconciliation$action, "close_failed")) stop("Invalid FAILED transition.")
        active <- complete <- pointer <- NULL
        pending_hash <- NULL
        phase <- "IDLE"
      } else {
        stop("Unknown selector transition.")
      }
    }
    previous_hash <- hash
  }
  actual <- list.files(file.path(registry, "generations"), all.files = TRUE, no.. = TRUE)
  if (any(!actual %in% generations) || any(!required %in% actual)) {
    stop("Orphan or missing generation directory.")
  }
  for (name in actual) {
    path <- file.path(registry, "generations", name)
    rfa_no_links(path)
    if (!rfa_directory(path)) stop("Generation must be a directory.")
  }
  current <- rfs_current(registry)
  if (!identical(current, published) &&
      !(identical(phase, "INTENT") && identical(current, pointer))) {
    stop("Current pointer differs from journal.")
  }
  staging <- file.path(registry, "pointer.tmp.rds")
  rfa_no_links(staging)
  if (file.exists(staging) && (!identical(phase, "INTENT") ||
      !identical(rfg_read(staging), pointer))) stop("Pointer staging differs.")
  list(active = active, phase = phase, published = published, pointer = pointer,
       pending_hash = pending_hash, complete = complete, current = current,
       previous_hash = previous_hash, count = length(files),
       completed_bindings = completed_bindings)
}

rfs_reconciliation <- function(x, pending, hash) {
  rfs_keys(x, c("pending_hash", "generation", "action", "quiescent", "reason", "reconciliations"))
  if (!identical(x$pending_hash, hash) || !identical(x$generation, pending$generation) ||
      !identical(x$quiescent, TRUE) || !is.list(x$reconciliations) ||
      is.object(x$reconciliations) || !is.null(dim(x$reconciliations)))
    stop("Invalid reconciliation binding.")
  rfg_text(x$reason)
  rfg_text(x$action)
  if (!x$action %in% c("close_failed", "resume_incomplete", "publish_complete") ||
      (x$action != "resume_incomplete" && length(x$reconciliations))) {
    stop("Invalid reconciliation action.")
  }
  invisible(TRUE)
}

rfs_append <- function(registry, state, type, definition = NULL,
                       ready = NULL, reconciliation = NULL) {
  i <- as.integer(state$count + 1L)
  pending <- state$active
  event <- list(schema = "reflow_artifact_selection_event_1", sequence = i,
    previous = state$previous_hash, type = type,
    pending_hash = if (type == "PENDING") NULL else state$pending_hash,
    generation = if (type == "PENDING")
      file.path(registry, "generations", sprintf("g%09d", i)) else pending$generation,
    definition = definition, ready = ready,
    old_pointer = if (type == "PENDING") state$published else pending$old_pointer,
    reconciliation = reconciliation)
  tmp <- file.path(registry, "event.tmp.rds")
  path <- rfs_event_path(registry, i)
  if (file.exists(tmp) || file.exists(path)) stop("Event destination occupied.")
  saveRDS(event, tmp, version = 2)
  rfs_checkpoint(paste0(type, "_STAGED"))
  if (!file.rename(tmp, path)) stop("Event rename failed; preserve evidence.")
  rfs_checkpoint(type)
  rfs_read(registry)
}

rfs_complete <- function(plan, registry, state, reconciliation) {
  definition <- rfg_definition(plan)
  if (!identical(definition, state$active$definition)) stop("Pending request changed.")
  binding <- rfs_history(state$active$generation)
  saved <- rfg_read(file.path(state$active$generation, "definition.rds"))
  saved$previous <- NULL
  if (!identical(saved, definition)) stop("Completed graph definition differs.")
  reflow_artifact_plan_resume(plan, state$active$generation)
  if (state$phase == "PENDING") {
    state <- rfs_append(registry, state, "COMPLETED", ready = binding$ready_hash,
                        reconciliation = reconciliation)
  }
  if (!identical(binding$ready_hash, state$complete$ready)) stop("Completed READY changed.")
  if (state$phase == "COMPLETED") {
    state <- rfs_append(registry, state, "INTENT", ready = binding$ready_hash,
                        reconciliation = reconciliation)
  }
  pointer <- state$pointer
  current <- rfs_current(registry)
  if (!identical(current, state$active$old_pointer) && !identical(current, pointer)) {
    stop("Pointer changed before publication.")
  }
  tmp <- file.path(registry, "pointer.tmp.rds")
  if (!identical(current, pointer)) {
    if (!file.exists(tmp)) {
      saveRDS(pointer, tmp, version = 2)
      rfs_checkpoint("POINTER_STAGED")
    }
    if (!identical(rfg_read(tmp), pointer)) stop("Pointer staging differs.")
    if (!identical(rfg_definition(plan), definition)) stop("Request changed before publication.")
    rfs_history(state$active$generation, binding$ready_hash)
    if (!identical(rfs_current(registry), current)) stop("Pointer changed before rename.")
    if (!file.rename(tmp, file.path(registry, "current.rds"))) stop("Pointer rename failed.")
    rfs_checkpoint("POINTER_RENAMED")
  } else if (file.exists(tmp)) {
    stop("Unexpected staging after pointer replacement.")
  }
  if (!identical(rfg_definition(plan), definition)) stop("Request changed after publication.")
  rfs_append(registry, state, "PUBLISHED", ready = binding$ready_hash,
             reconciliation = reconciliation)
  pointer
}

rfs_execute <- function(plan, registry, initialize, reconciliation, verify_pointer) {
  if (!identical(Sys.info()[["sysname"]], "Linux")) stop("Selector requires Linux qualification.")
  if (!identical(initialize, TRUE) && !identical(initialize, FALSE))
    stop("Plain initialize flag required.")
  registry <- rfs_path(registry)
  lock <- paste0(registry, ".lock")
  rfa_no_links(lock)
  if (!dir.create(lock, showWarnings = FALSE)) stop("Registry locked; reconcile owner externally.")
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  definition <- rfg_definition(plan)
  for (stage in plan$stages) {
    args <- rfg_walk(stage$declaration$args, function(x) "deferred-artifact")
    rf_walk(args, function(x) {
      input <- rfa_canonical(x$path)
      if (rfa_within(registry, input) || rfa_within(input, registry) ||
          rfa_within(lock, input) || rfa_within(input, lock))
        stop("Registry overlaps tracked input.")
      x
    })
  }
  if (initialize) {
    if (!is.null(reconciliation) || !is.null(verify_pointer) || file.exists(registry)) {
      stop("Initialization requires an absent registry.")
    }
    if (!dir.create(registry) || !dir.create(file.path(registry, "events")) ||
        !dir.create(file.path(registry, "generations"))) stop("Registry initialization failed.")
    saveRDS(list(schema = "reflow_artifact_registry_1", registry = registry,
                 event_staging = "event.tmp.rds", pointer_staging = "pointer.tmp.rds"),
            file.path(registry, "registry.rds"), version = 2)
  }
  state <- rfs_read(registry)
  # Every COMPLETED event remains an integrity obligation, even after FAILED.
  # Each verification releases its acquired ancestry locks before graph APIs.
  for (binding in state$completed_bindings) {
    rfs_history(binding$directory, binding$ready_hash, binding$definition)
  }
  if (!is.null(verify_pointer)) {
    if (state$phase != "IDLE" || !identical(verify_pointer, state$current)) {
      stop("Incomplete or stale selection.")
    }
    saved <- rfg_read(file.path(verify_pointer$generation, "definition.rds"))
    saved$previous <- NULL
    if (!identical(saved, definition) ||
        !identical(rf_hash(definition), verify_pointer$request_hash)) {
      stop("Selection request changed.")
    }
    return(reflow_artifact_plan_resume(plan, verify_pointer$generation))
  }
  if (!is.null(state$active)) {
    if (is.null(reconciliation))
      stop("Unresolved pending selection requires explicit reconciliation.")
    rfs_reconciliation(reconciliation, state$active, state$pending_hash)
    if (reconciliation$action == "close_failed") {
      if (!identical(state$current, state$active$old_pointer) ||
          file.exists(file.path(registry, "pointer.tmp.rds"))) {
        stop("Publication has advanced; only publish_complete is allowed.")
      }
      rfs_append(registry, state, "FAILED", reconciliation = reconciliation)
      return(invisible(NULL))
    }
    if (!identical(definition, state$active$definition)) stop("Pending request changed.")
    if (reconciliation$action == "resume_incomplete") {
      if (state$phase != "PENDING" ||
          !rfa_directory(state$active$generation) ||
          file.exists(file.path(state$active$generation, "READY.rds"))) {
        stop("Incomplete graph required; absent graph must be closed failed.")
      }
      reflow_artifact_plan_resume(plan, state$active$generation,
                                  reconciliations = reconciliation$reconciliations)
    }
    return(rfs_complete(plan, registry, state, reconciliation))
  }
  if (!is.null(reconciliation)) stop("No pending selection to reconcile.")
  if (!is.null(state$published)) {
    saved <- rfg_read(file.path(state$published$generation, "definition.rds"))
    saved$previous <- NULL
    if (identical(saved, definition)) {
      reflow_artifact_plan_resume(plan, state$published$generation)
      return(state$published)
    }
  }
  state <- rfs_append(registry, state, "PENDING", definition = definition)
  prior <- if (is.null(state$published)) NULL else state$published$generation
  reflow_artifact_plan_run(plan, state$active$generation, previous = prior)
  rfs_checkpoint("GRAPH_COMPLETE")
  rfs_complete(plan, registry, state, NULL)
}
