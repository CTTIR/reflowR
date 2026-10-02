#' Reference a declared artifact in a dependency plan
#'
#' References identify logical inventory names, not arbitrary output paths.
#' @param stage Producer stage identifier.
#' @param artifact One logical artifact name in the producer inventory.
#' @param read Read the verified artifact as RDS; otherwise pass its path.
#' @return A deferred artifact reference.
#' @export
reflow_artifact_ref <- function(stage, artifact, read = FALSE) {
  rfg_text(stage)
  rfg_text(artifact)
  if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", stage) ||
    !is.logical(read) || is.object(read) || !is.null(dim(read)) ||
    length(read) != 1L || is.na(read)) {
    stop("Invalid artifact reference.")
  }
  structure(list(stage = stage, artifact = artifact, read = read),
    class = "reflow_artifact_ref"
  )
}

#' Declare a deferred artifact writer
#'
#' This additive API permits [reflow_artifact_ref()] in ordinary arguments.
#' Static declarations are validated immediately; future artifact paths are
#' resolved only after their producers have been authenticated. Scientific
#' methods remain in trusted installed exports. No writer runs here.
#' @param id Unique stage identifier.
#' @param package,fun,args,outputs,inventory,allow_empty,seed,packages,runtime_files
#'   See [reflow_artifact_spec()].
#' @return A deferred writer declaration.
#' @export
reflow_artifact_stage <- function(
  id, package, fun, args = list(), outputs, inventory,
  allow_empty = character(), seed = NULL, packages = character(),
  runtime_files = character()
) {
  rfg_text(id)
  if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", id)) stop("Invalid stage id.")
  rfa_paths(id)
  placeholder <- rfg_walk(args, function(x) "deferred-artifact")
  spec <- reflow_artifact_spec(
    package, fun, placeholder, outputs, inventory,
    allow_empty, seed, packages, runtime_files
  )
  spec$args <- args
  structure(list(id = id, declaration = spec), class = "reflow_artifact_stage")
}

#' Declare a serial artifact dependency plan
#'
#' Declaration order is immaterial: a stable identifier order breaks ties among
#' available stages. This dependency core is not a resource scheduler or a
#' targets adapter. Existing files use [reflow_imaging_input()]; no external
#' execution is inferred from an input path. Plain strings are configuration.
#' @param ... Deferred [reflow_artifact_stage()] declarations.
#' @return A canonical acyclic plan, without executing writers.
#' @export
reflow_artifact_plan <- function(...) {
  stages <- list(...)
  if (!length(stages)) stop("Supply at least one artifact stage.")
  for (s in stages) {
    if (!identical(class(s), "reflow_artifact_stage") ||
      !identical(names(s), c("id", "declaration"))) {
      stop("Invalid deferred stage.")
    }
    rebuilt <- do.call(
      reflow_artifact_stage,
      c(list(id = s$id), rfg_spec_args(s$declaration))
    )
    if (!identical(s, rebuilt)) stop("Noncanonical deferred stage.")
  }
  ids <- vapply(stages, `[[`, character(1), "id")
  if (anyDuplicated(ids) ||
    .Platform$OS.type == "windows" && anyDuplicated(tolower(ids))) {
    stop("Duplicate stage ids.")
  }
  names(stages) <- ids
  dependencies <- lapply(stages, function(s) {
    found <- character()
    rfg_walk(s$declaration$args, function(x) {
      if (!x$stage %in% ids ||
        !x$artifact %in% names(stages[[x$stage]]$declaration$inventory)) {
        stop("Unknown artifact producer or logical name.")
      }
      found <<- c(found, x$stage)
      x
    })
    sort(unique(found))
  })
  ordered <- character()
  while (length(ordered) < length(stages)) {
    available <- sort(setdiff(ids[vapply(dependencies, function(x) {
      all(x %in% ordered)
    }, logical(1))], ordered))
    if (!length(available)) stop("Cyclic artifact dependencies.")
    ordered <- c(ordered, available)
  }
  structure(
    list(
      schema = "reflow_artifact_plan_1", stages = stages[ordered],
      dependencies = dependencies[ordered]
    ),
    class = "reflow_artifact_plan"
  )
}

rfg_text <- function(x) {
  if (!is.character(x) || is.object(x) || !is.null(dim(x)) ||
    length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("Expected plain text.")
  }
}

rfg_spec_args <- function(spec) {
  if (!identical(class(spec), "reflow_artifact_spec") ||
    !identical(spec$schema, "reflow_artifact_1")) {
    stop("Invalid declaration.")
  }
  x <- unclass(spec)
  x$schema <- NULL
  x
}

rfg_walk <- function(x, reference) {
  if (inherits(x, "reflow_artifact_ref")) {
    rebuilt <- do.call(reflow_artifact_ref, unclass(x))
    if (!identical(x, rebuilt)) stop("Noncanonical artifact reference.")
    return(reference(x))
  }
  if (inherits(x, "reflow_imaging_ref")) stop("Object references are unsupported.")
  if (inherits(x, "reflow_imaging_input")) {
    return(x)
  }
  if (isS4(x) || !(is.null(x) || is.atomic(x) || is.list(x))) {
    stop("Unsupported argument object.")
  }
  if (is.list(x)) x[] <- lapply(x, rfg_walk, reference = reference)
  x
}

rfg_definition <- function(plan) {
  if (!identical(class(plan), "reflow_artifact_plan") ||
    !identical(plan$schema, "reflow_artifact_plan_1") ||
    !identical(plan, do.call(reflow_artifact_plan, unname(plan$stages)))) {
    stop("Noncanonical artifact plan.")
  }
  signatures <- lapply(plan$stages, function(s) {
    spec <- s$declaration
    spec$args <- rfg_walk(spec$args, function(x) "deferred-artifact")
    rfa_signature(spec)
  })
  list(
    schema = "reflow_artifact_graph_definition_1", plan = plan,
    signatures = signatures
  )
}
