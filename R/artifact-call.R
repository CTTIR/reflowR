#' Call one installed export and save its value
#'
#' A generic artifact writer: calls exactly one exported function of an
#' installed package with the given arguments and saves the returned object as
#' RDS. Use it as the writer of a [reflow_artifact_stage()] (see
#' [reflow_call_stage()]) so that configured calculations need no writer code
#' of their own. Artifact references in `args` are resolved by the executor
#' before this function runs; with `read = TRUE` they arrive as objects.
#'
#' The function is trusted and is not sandboxed. It must not write files of
#' its own. Declare its package in the stage `packages` so that its installed
#' files are fingerprinted; [reflow_call_stage()] does this.
#'
#' A tab-separated copy is written when `tsv` is a path. A data frame whose
#' columns are all atomic vectors is written with a header row, `NA` for
#' missing values and double quotes only around fields that contain a tab,
#' line break or double quote. Any other value gives an empty file, so the
#' declared inventory stays fixed; declare that artifact in `allow_empty`.
#' The RDS file is the exact value; the table is a readable copy.
#' @param fun One string `"package::function"` naming an exported function.
#' @param args Uniquely named list of arguments for `fun`.
#' @param path Output path of the RDS file.
#' @param tsv Optional output path of the tab-separated copy, or NULL.
#' @return The output paths, invisibly.
#' @examples
#' out <- tempfile(fileext = ".rds")
#' reflow_call_writer("utils::head", list(x = datasets::mtcars, n = 3), out)
#' readRDS(out)
#' @export
reflow_call_writer <- function(fun, args = list(), path, tsv = NULL) {
  target <- rfc_fun(fun)
  rfc_args(args)
  rfg_text(path)
  if (!is.null(tsv)) {
    rfg_text(tsv)
    if (identical(normalizePath(tsv, mustWork = FALSE),
                  normalizePath(path, mustWork = FALSE))) {
      stop("path and tsv must differ.")
    }
  }
  f <- getExportedValue(target[[1L]], target[[2L]])
  if (!is.function(f)) stop("Not an exported function: ", fun)
  value <- do.call(f, args)
  saveRDS(value, path)
  if (!is.null(tsv)) rfc_tsv(value, tsv)
  invisible(c(path, tsv))
}

#' Declare a stage that calls one installed export
#'
#' Builds the [reflow_artifact_stage()] for [reflow_call_writer()]: the stage
#' saves the value of `fun` as the artifact `value` (`value.rds`) and a
#' tab-separated copy as `table` (`value.tsv`, empty for values that are not
#' plain data frames). The package of `fun` and any `packages` are
#' fingerprinted. Refer to the value downstream with
#' `reflow_artifact_ref(id, "value", read = TRUE)`.
#' @param id Stage identifier, unique within a plan even when letter case is
#'   ignored.
#' @param fun One string `"package::function"` naming an exported function.
#' @param args Uniquely named list of arguments; may contain
#'   [reflow_artifact_ref()] and [reflow_imaging_input()] values at any depth.
#' @param seed Explicit nonnegative integer seed, or NULL for a nonrandom call.
#' @param packages Additional installed packages to fingerprint, such as
#'   optional packages that `fun` uses.
#' @return A deferred writer declaration.
#' @examples
#' stage <- reflow_call_stage("top", "utils::head", list(x = 1:10, n = 3))
#' stage$declaration$inventory
#' @export
reflow_call_stage <- function(id, fun, args = list(), seed = NULL,
                              packages = character()) {
  target <- rfc_fun(fun)
  rfc_args(args)
  if (!is.character(packages) || is.object(packages) || anyNA(packages) ||
    any(!nzchar(packages))) {
    stop("packages must contain package names.")
  }
  reflow_artifact_stage(
    id, "reflowR", "reflow_call_writer",
    args = list(fun = fun, args = args),
    outputs = list(
      path = list(path = "value.rds", type = "file"),
      tsv = list(path = "value.tsv", type = "file")
    ),
    inventory = c(value = "value.rds", table = "value.tsv"),
    allow_empty = "table", seed = seed,
    packages = sort(unique(c(target[[1L]], packages)))
  )
}

rfc_fun <- function(fun) {
  rfg_text(fun)
  parts <- regmatches(fun, regexec(
    "^([A-Za-z][A-Za-z0-9.]*[A-Za-z0-9])::([A-Za-z.][A-Za-z0-9._]*)$", fun
  ))[[1L]]
  if (length(parts) != 3L) stop("fun must be one 'package::function' string.")
  parts[2:3]
}

rfc_args <- function(args) {
  if (!is.list(args) || is.object(args) || !is.null(dim(args))) {
    stop("args must be a plain list.")
  }
  if (length(args)) rfa_names(args, "args")
}

rfc_tsv <- function(value, path) {
  tabular <- is.data.frame(value) && ncol(value) > 0L &&
    all(vapply(value, function(x) {
      is.atomic(x) && is.null(dim(x)) || is.factor(x)
    }, logical(1)))
  if (!tabular) {
    file.create(path)
    return(invisible(path))
  }
  field <- function(x) {
    x <- as.character(x)
    quote <- !is.na(x) & grepl("[\t\r\n\"]", x)
    x[quote] <- paste0("\"", gsub("\"", "\"\"", x[quote], fixed = TRUE), "\"")
    x[is.na(x)] <- "NA"
    x
  }
  columns <- lapply(value, function(x) {
    if (inherits(x, c("Date", "POSIXt"))) x <- format(x)
    field(x)
  })
  lines <- c(
    paste(field(names(value)), collapse = "\t"),
    if (nrow(value)) do.call(paste, c(unname(columns), sep = "\t"))
  )
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(lines), con, sep = "\n", useBytes = TRUE)
  invisible(path)
}
