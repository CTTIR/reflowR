#' Declare one immutable file-producing bundle
#'
#' Calls one trusted installed export, substituting only declared top-level
#' output arguments. The writer's return value is discarded. This is not a
#' sandbox, scheduler, native-process supervisor or complete artifact DAG.
#' Plain strings are configuration; files must use [reflow_imaging_input()].
#' Paths reject symbolic links, special files, reserved Windows components and
#' trailing dots/spaces. Unicode spelling is preserved, not normalized. Parents
#' of output paths are created; declared output directories are writer-created.
#' Installed package resources (including templates), tracked inputs and
#'   declared
#' external runtime files are fingerprinted. Undeclared ambient state is not.
#' @param package,fun Installed package and exported writer function.
#' @param args Uniquely named ordinary arguments, excluding output arguments.
#' @param outputs Named list keyed by output argument, each an exact list with
#'   `path` (relative to the bundle) and `type` (`file` or `directory`).
#' @param inventory Nonempty named character vector of logical artifact names
#'   mapped to exact relative file paths. Unexpected directories also fail.
#' @param allow_empty Logical artifact names permitted to contain zero bytes.
#' @param seed Explicit nonnegative integer seed, or NULL for a nonrandom
#'   writer.
#' @param packages Additional installed packages to fingerprint.
#' @param runtime_files Existing external runtime files to fingerprint.
#'   The caller
#'   must ensure the writer actually dispatches these binaries/resources.
#' @return A versioned declaration; no writer is invoked.
#' @export
reflow_artifact_spec <- function(
  package, fun, args = list(), outputs, inventory, allow_empty = character(),
  seed = NULL, packages = character(), runtime_files = character()
) {
  stage <- reflow_imaging_stage("bundle", package, fun, args, seed)
  rfa_names(outputs, "outputs")
  if (!is.list(outputs) || is.object(outputs) || !is.null(dim(outputs)) ||
        !length(outputs))
    stop("outputs must be a nonempty named list.")
  if (length(
    intersect(
      names(outputs),
      names(args)
    )
  ))
    stop("Output arguments overlap args.")
  for (out in outputs) {
    if (!is.list(out) || is.object(out) || !is.null(dim(out)) ||
      !identical(
        sort(names(out)),
        c("path", "type")
      )) {
      stop("Each output requires exactly path and type.")
    }
    rfa_paths(out$path)
    if (length(out$path) !=
          1L || !identical(out$type, "file") &&
          !identical(out$type, "directory")) {
      stop("Invalid output declaration.")
    }
  }
  roots <- vapply(
    outputs, `[[`, character(1),
    "path"
  )
  rfa_paths(roots)
  if (rfa_overlap(roots))
    stop("Output roots overlap.")
  rfa_names(inventory, "inventory")
  rfa_paths(inventory)
  if (!length(inventory) ||
        rfa_overlap(unname(inventory)))
    stop("Invalid file inventory.")
  for (file in inventory) {
    owners <- vapply(
      outputs, function(out) {
        if (out$type == "file")
          file == out$path else startsWith(file, paste0(out$path, "/"))
      }, logical(1)
    )
    if (sum(owners) !=
          1L)
      stop("Every artifact requires exactly one declared output owner.")
  }
  for (out in outputs) {
    owned <- if (out$type == "file") {
      inventory == out$path
    } else {
      startsWith(inventory, paste0(out$path, "/"))
    }
    if (!any(owned))
      stop("An output must own at least one artifact.")
  }
  if (!is.character(allow_empty) ||
        anyNA(allow_empty) ||
        anyDuplicated(allow_empty) ||
        !all(allow_empty %in% names(inventory))) {
    stop("allow_empty must contain distinct declared logical names.")
  }
  if (!is.character(runtime_files) ||
        anyNA(runtime_files) ||
        anyDuplicated(runtime_files)) {
    stop("Invalid runtime_files.")
  }
  runtime_files <- vapply(
    runtime_files, function(path) {
      rfa_no_links(path)
      if (!rfa_regular(path))
        stop("Runtime inputs must be regular files.")
      rfa_canonical(path)
    }, character(1),
    USE.NAMES = FALSE
  )
  rf_walk(
    args, function(x) {
      if (inherits(x, "reflow_imaging_ref"))
        stop("Upstream references are outside this bundle API.")
      x
    }
  )
  reflow_imaging_plan(stage, packages = packages)
  structure(
    list(
      schema = "reflow_artifact_1", package = package, fun = fun,
      args = args, outputs = outputs, inventory = inventory,
      allow_empty = allow_empty,
      seed = seed, packages = packages, runtime_files = runtime_files
    ),
    class = "reflow_artifact_spec"
  )
}

rfa_names <- function(x, label) {
  if (is.null(names(x)) ||
        anyNA(names(x)) ||
        any(!nzchar(names(x))) ||
        anyDuplicated(names(x))) {
    stop(label, " requires unique nonempty names.")
  }
}

rfa_paths <- function(x) {
  if (!is.character(x) || is.object(x) || !is.null(dim(x)) || anyNA(x) ||
        any(!nzchar(x)) || any(grepl("[[:cntrl:]:]", x)) ||
        any(grepl("\\", x, fixed = TRUE)) || any(startsWith(x, "/")) ||
        any(grepl("(^|/)[.]{1,2}(/|$)|//|/$", x)) ||
        anyDuplicated(tolower(x))) {
    stop("Invalid or case-colliding relative paths.")
  }
  components <- unlist(strsplit(x, "/", fixed = TRUE), use.names = FALSE)
  if (any(grepl('[<>"|?*]|[. ]$', components)) ||
        any(grepl("^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])([.]|$)",
                  toupper(components)))) {
    stop("Reserved or ambiguous path component.")
  }
  all_paths <- x
  for (file in x) {
    parent <- dirname(file)
    while (parent != ".") {
      all_paths <- c(all_paths, parent)
      parent <- dirname(parent)
    }
  }
  if (anyDuplicated(tolower(unique(all_paths))))
    stop("Case-colliding path ancestors.")
}

rfa_overlap <- function(x) {
  x <- tolower(x)
  any(
    vapply(
      seq_along(x),
      function(i) {
        any(startsWith(x[-i], paste0(x[i], "/")))
      }, logical(1)
    )
  )
}

rfa_no_links <- function(path) {
  path <- path.expand(path)
  if (!grepl("^(/|[A-Za-z]:[/\\\\])", path))
    path <- file.path(getwd(), path)
  repeat {
    link <- Sys.readlink(path)
    if (!is.na(link) &&
          nzchar(link))
      stop("Symbolic links are not allowed: ", path)
    parent <- dirname(path)
    if (identical(parent, path))
      break
    path <- parent
  }
  invisible(TRUE)
}

rfa_canonical <- function(path) {
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

rfa_within <- function(path, root, insensitive =
                         .Platform$OS.type == "windows" ||
                         Sys.info()[["sysname"]] == "Darwin") {
  path <- gsub("\\", "/", path, fixed = TRUE)
  root <- gsub("\\", "/", root, fixed = TRUE)
  path <- sub("/+$", "", path)
  root <- sub("/+$", "", root)
  if (insensitive) {
    path <- tolower(path)
    root <- tolower(root)
  }
  identical(path, root) || startsWith(path, paste0(root, "/"))
}

rfa_directory <- function(path) {
  type <- fs::file_info(path, follow = FALSE, fail = FALSE)$type
  !is.na(type) & as.character(type) == "directory"
}

rfa_regular <- function(path) {
  type <- fs::file_info(path, follow = FALSE, fail = FALSE)$type
  length(type) == 1L && !is.na(type) && as.character(type) == "file"
}

rfa_read <- function(path) {
  rfa_no_links(path)
  if (!rfa_regular(path)) stop("Receipt must be a regular file.")
  readRDS(path)
}

rfa_tree <- function(path) {
  rfa_no_links(path)
  if (!rfa_directory(path))
    stop("Missing directory: ", path)
  entries <- character()
  pending <- path
  while (length(pending)) {
    directory <- pending[[1L]]
    pending <- pending[-1L]
    children <- list.files(directory, all.files = TRUE,
                           full.names = TRUE, no.. = TRUE)
    for (entry in children) rfa_no_links(entry)
    entries <- c(entries, children)
    pending <- c(pending, children[rfa_directory(children)])
  }
  files <- entries[!rfa_directory(entries)]
  if (any(
    !vapply(
      files, function(file) rfa_regular(file),
      logical(1)
    )
  )) {
    stop("Only regular files are supported.")
  }
  list(
    files = sort(
      substring(
        files, nchar(path) +
          2L
      )
    ),
    directories = sort(
      substring(
        entries[rfa_directory(entries)],
        nchar(path) +
          2L
      )
    )
  )
}

rfa_signature <- function(spec) {
  if (!inherits(spec, "reflow_artifact_spec") ||
        !identical(spec$schema, "reflow_artifact_1")) {
    stop("Unsupported artifact schema.")
  }
  args <- unclass(spec)
  args$schema <- NULL
  rebuilt <- do.call(reflow_artifact_spec, args)
  if (!identical(spec, rebuilt))
    stop("Noncanonical artifact declaration.")
  tracked <- rf_walk(
    spec$args, function(x) {
      rfa_no_links(x$path)
      tree <- NULL
      if (rfa_directory(x$path)) {
        tree <- rfa_tree(x$path)
      } else if (!rfa_regular(x$path)) {
        stop("Tracked inputs must be regular files or directories.")
      }
      list(input = x, tree = tree)
    }
  )
  stage <- reflow_imaging_stage("bundle", spec$package, spec$fun,
                                spec$args, spec$seed)
  database <- utils::installed.packages()
  direct <- unique(c(spec$package, spec$packages))
  if (!all(direct %in% rownames(database))) stop("Missing declared package.")
  dependencies <- tools::package_dependencies(direct, db = database,
                                              which = c("Depends",
                                                        "Imports", "LinkingTo"),
                                              recursive = TRUE)
  packages <- sort(unique(c("reflowR", "digest", direct,
                            unlist(dependencies, use.names = FALSE))))
  resources <- lapply(packages, function(package) {
    path <- rfa_canonical(find.package(package))
    tree <- rfa_tree(path)
    list(path = path, directories = tree$directories,
         files = stats::setNames(vapply(file.path(path, tree$files),
                                        rf_file_hash, character(1)),
                                 tree$files))
  })
  names(resources) <- packages
  base <- rf_signature(reflow_imaging_plan(stage, packages = spec$packages))
  if (!identical(names(base$packages), packages)) {
    stop("Package closure changed.")
  }
  list(
    schema = "reflow_artifact_definition_1", spec = spec, base = base,
    resources = resources, tracked = tracked, runtime = stats::setNames(
      vapply(spec$runtime_files, rf_file_hash, character(1)),
      spec$runtime_files
    )
  )
}

rfa_inventory <- function(spec, bundle) {
  tree <- rfa_tree(bundle)
  expected <- sort(unname(spec$inventory))
  directories <- character()
  for (file in expected) {
    parent <- dirname(file)
    while (parent != ".") {
      directories <- c(directories, parent)
      parent <- dirname(parent)
    }
  }
  if (!identical(tree$files, expected) ||
        !identical(tree$directories, sort(unique(directories)))) {
    stop("Artifact inventory differs.")
  }
  paths <- file.path(bundle, spec$inventory)
  bytes <- unname(file.info(paths)$size)
  if (anyNA(bytes) ||
    any(
      bytes == 0 & !names(spec$inventory) %in%
        spec$allow_empty
    )) {
    stop("Unexpected empty or unavailable artifact.")
  }
  data.frame(
    name = names(spec$inventory),
    path = unname(spec$inventory),
    bytes = bytes, sha256 = unname(vapply(paths, rf_file_hash, character(1))),
    stringsAsFactors = FALSE
  )
}

#' Execute or verify one declared artifact bundle
#'
#' Fresh runs require an absent destination with an existing parent. Resume
#' revalidates a READY bundle without invoking its writer. Failed/interrupted
#' attempts require explicit caller reconciliation; no subprocess absence is
#' inferred from an R error or owner PID. Stale locks require manual inspection.
#' Orphan attempts with missing state always require a new run after external
#' reconciliation; supplying retry arguments does not bypass this refusal.
#' Receipts use atomic rename visibility, not a power-loss durability guarantee.
#' Writers are trusted: observable input changes are detected, but transient
#' restored changes and escaped subprocesses are not prevented.
#' @param spec A [reflow_artifact_spec()].
#' @param directory New run directory, or an existing directory for resume.
#' @param reconciled_attempt Exact latest failed/interrupted attempt identifier.
#' @param reconciliation Nonempty caller assertion explaining that all writer
#'   and subprocess activity has stopped. Required together with the identifier
#'   before a fresh retry. This is not automatic process verification.
#' @return An authenticated descriptor with stable bundle path and file
#'   manifest.
#' @export
reflow_artifact_run <- function(spec, directory) {
  rfa_no_links(directory)
  if (!rfa_directory(dirname(directory)))
    stop("Destination parent must exist.")
  destination <- file.path(rfa_canonical(dirname(directory)),
                           basename(directory))
  rf_walk(
    spec$args, function(x) {
      if (identical(destination, rfa_canonical(x$path)) ||
            rfa_directory(x$path) && rfa_within(destination,
                                                rfa_canonical(x$path))) {
        stop("Run destination overlaps tracked input.")
      }
      x
    }
  )
  signature <- rfa_signature(spec)
  if (file.exists(directory) ||
        !dir.create(directory, showWarnings = FALSE)) {
    stop("Fresh destination required.")
  }
  rfa_execute(
    spec, rfa_canonical(directory),
    signature, NULL, NULL
  )
}

#' @rdname reflow_artifact_run
#' @export
reflow_artifact_resume <- function(spec, directory,
                                   reconciled_attempt = NULL,
                                   reconciliation = NULL) {
  rfa_no_links(directory)
  rfa_execute(
    spec, rfa_canonical(directory),
    NULL, reconciled_attempt, reconciliation
  )
}

rfa_attempt <- function(x) {
  is.character(x) &&
    length(x) ==
      1L && !is.na(x) &&
    grepl("^attempt-[A-Za-z0-9]+$", x)
}

rfa_descriptor <- function(spec, directory, receipt, definition) {
  fields <- c("schema", "status", "attempt", "definition_hash", "inventory")
  if (!is.list(receipt) ||
    !identical(
      sort(names(receipt)),
      sort(fields)
    ) ||
    !identical(receipt$schema, "reflow_artifact_ready_1") ||
    !identical(receipt$status, "READY") ||
    !rfa_attempt(receipt$attempt) ||
    !identical(receipt$definition_hash, rf_hash(definition))) {
    stop("Malformed READY receipt.")
  }
  attempt <- file.path(directory, receipt$attempt)
  rfa_no_links(attempt)
  rfa_no_links(file.path(attempt, "receipt.rds"))
  if (!identical(
    rfa_read(file.path(attempt, "receipt.rds")),
    receipt
  )) {
    stop("Accepted attempt receipt differs.")
  }
  bundle <- file.path(attempt, "bundle")
  if (!identical(
    rfa_inventory(spec, bundle),
    receipt$inventory
  ))
    stop("Corrupt accepted bundle.")
  structure(
    list(
      schema = "reflow_artifact_descriptor_1", directory = directory,
      bundle = bundle, definition_hash = receipt$definition_hash,
      inventory = receipt$inventory
    ),
    class = "reflow_artifact_descriptor"
  )
}

rfa_execute <- function(spec, directory, signature, reconciled_attempt,
                        reconciliation) {
  lock <- file.path(directory, ".lock")
  if (!dir.create(lock, showWarnings = FALSE))
    stop("Run locked; reconcile owner before release.")
  on.exit(
    unlink(lock, recursive = TRUE),
    add = TRUE
  )
  rf_write(
    list(
      pid = Sys.getpid(), host = Sys.info()[["nodename"]], time = Sys.time()
    ),
    file.path(lock, "owner.rds")
  )
  definition_path <- file.path(directory, "definition.rds")
  ready_path <- file.path(directory, "READY.rds")
  state_path <- file.path(directory, "state.rds")
  current <- rfa_signature(spec)
  if (!is.null(signature)) {
    if (!identical(signature, current))
      stop("Definition changed before initialization.")
    rf_write(current, definition_path)
  }
  rfa_no_links(definition_path)
  saved <- rfa_read(definition_path)
  if (!identical(current, saved))
    stop("Declaration, inputs or runtime changed.")
  rfa_no_links(ready_path)
  if (file.exists(ready_path)) {
    rfa_descriptor(
      spec, directory, rfa_read(ready_path),
      saved
    )
    if (!identical(
      rfa_signature(spec),
      saved
    ))
      stop("Definition changed during verification.")
    return(
      invisible(
        rfa_descriptor(
          spec, directory, rfa_read(ready_path),
          saved
        )
      )
    )
  }
  rfa_no_links(state_path)
  attempts <- list.files(directory, pattern = "^attempt-", all.files = TRUE)
  if (!file.exists(state_path) && length(attempts)) {
    stop("Orphan attempts without state; reconcile processes before a new run.")
  }
  if (file.exists(state_path)) {
    old <- rfa_read(state_path)
    if (!is.list(old) ||
          !rfa_attempt(old$attempt) ||
          !old$status %in% c("FAILED", "RUNNING") ||
          !identical(old$definition_hash, rf_hash(saved))) {
      stop("Malformed attempt state.")
    }
    expected_fields <- c("status", "attempt", "definition_hash",
                         "reconciliation")
    if (identical(old$status, "FAILED"))
      expected_fields <- c(expected_fields, "error")
    if (!identical(
      sort(names(old)),
      sort(expected_fields)
    ))
      stop("Malformed attempt fields.")
    old_receipt <- file.path(directory, old$attempt, "receipt.rds")
    rfa_no_links(old_receipt)
    if (!identical(
      rfa_read(old_receipt),
      old
    ))
      stop("Attempt state differs from receipt.")
    if (!identical(reconciled_attempt, old$attempt) ||
          !is.character(reconciliation) ||
          length(reconciliation) !=
            1L || is.na(reconciliation) ||
          !nzchar(trimws(reconciliation))) {
      stop(
        "Retry requires reconciled_attempt and explicit process reconciliation."
      )
    }
  }
  attempt <- tempfile("attempt-", directory)
  if (!dir.create(attempt))
    stop("Cannot create attempt.")
  bundle <- file.path(attempt, "bundle")
  if (!dir.create(bundle))
    stop("Cannot create bundle.")
  state <- list(
    status = "RUNNING", attempt = basename(attempt),
    definition_hash = rf_hash(saved),
    reconciliation = reconciliation
  )
  rf_write(state, file.path(attempt, "receipt.rds"))
  rf_write(state, state_path)
  published <- FALSE
  fail <- function(e) {
    if (published)
      unlink(ready_path)
    state$status <- "FAILED"
    state$error <- conditionMessage(e)
    rf_write(state, file.path(attempt, "receipt.rds"))
    rf_write(state, state_path)
    stop(e)
  }
  tryCatch(
    {
      args <- rf_walk(
        spec$args, function(x) {
          if (x$read) readRDS(x$path) else x$path
        }
      )
      for (name in names(spec$outputs)) {
        path <- file.path(bundle, spec$outputs[[name]]$path)
        parent <- dirname(path)
        if (!rfa_directory(parent) &&
              !dir.create(parent, recursive = TRUE)) {
          stop("Cannot create output parent.")
        }
        args[[name]] <- path
      }
      stage <- reflow_imaging_stage("bundle", spec$package, spec$fun,
                                    seed = spec$seed)
      rf_call(stage, args)
      inventory <- rfa_inventory(spec, bundle)
      if (!identical(
        rfa_signature(spec),
        saved
      ))
        stop("Definition changed during writer call.")
      receipt <- list(
        schema = "reflow_artifact_ready_1", status = "READY",
        attempt = basename(attempt),
        definition_hash = rf_hash(saved),
        inventory = inventory
      )
      if (file.exists(ready_path))
        stop("Accepted receipt already exists.")
      rf_write(receipt, file.path(attempt, "receipt.rds"))
      # Check the complete descriptor before publishing the sole
      # accepted pointer.
      rfa_descriptor(spec, directory, receipt, saved)
      rf_write(receipt, ready_path)
      published <- TRUE
      invisible(rfa_descriptor(spec, directory, receipt, saved))
    }, error = fail, interrupt = fail
  )
}
