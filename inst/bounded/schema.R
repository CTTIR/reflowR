# Pure declaration checks shared by installed client and entry points.
rb_plain <- function(x) {
  if (is.null(x)) {
    return(TRUE)
  }
  if (is.environment(x) || is.function(x) || is.language(x) || isS4(x) ||
    typeof(x) %in% c("externalptr", "weakref")) {
    return(FALSE)
  }
  allowed <- c(
    "reflow_artifact_spec", "reflow_imaging_input",
    "reflow_imaging_plan", "reflow_imaging_stage", "data.frame",
    "Date", "POSIXct", "POSIXt", "factor", "ordered"
  )
  if (is.object(x) && !all(class(x) %in% allowed)) {
    return(FALSE)
  }
  if (is.list(x) && !all(vapply(x, rb_plain, logical(1)))) {
    return(FALSE)
  }
  attrs <- attributes(x)
  if (!is.null(attrs)) {
    attrs$class <- NULL
    if (!all(vapply(attrs, rb_plain, logical(1)))) {
      return(FALSE)
    }
  }
  TRUE
}
rb_keys <- function(x, keys) {
  is.list(x) && !is.object(x) && is.null(dim(x)) &&
    identical(sort(names(x)), sort(keys)) && rb_plain(x)
}
rb_request <- function(x) {
  keys <- c(
    "schema", "spec", "expected_definition", "directory", "resume", "reconciled_attempt",
    "reconciliation", "libraries", "package_path", "package_paths", "runtime_sha256",
      "runtime_observation",
    "channel_script"
  )
  if (!rb_keys(x, keys) || !identical(x$schema, 1L) ||
    !identical(class(x$spec), "reflow_artifact_spec") ||
    !identical(x$spec$schema, "reflow_artifact_1") ||
    !is.logical(x$resume) || length(x$resume) != 1L || is.na(x$resume)) {
    stop("Invalid bounded artifact request.")
  }
  invisible(TRUE)
}
