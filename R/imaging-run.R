#' Execute or resume a recorded imaging plan
#'
#' Runs are sequential and local. Every call result is saved to a new immutable
#' attempt directory. Receipts contain configuration, input, package, runtime,
#' resolved-argument and output hashes. Resume rejects changed definitions or
#' inputs and corrupt results, rather than silently reusing stale output. A
#' failed call can be retried with the identical plan; completed stages are
#' verified before reuse. Random stages require an explicit stage seed; the
#' caller's random state is restored after each call.
#'
#' The executor never initializes Git, publishes, installs packages or downloads
#' data. Called functions are trusted and can have side effects; this API does
#' not sandbox them. File-producing, distributed and resource-scheduled stages
#' are outside this initial object-returning contract. Package fingerprints cover
#' installed code, native libraries and dependency metadata, not external engines.
#' Common OpenMP/BLAS/MKL/Accelerate/BLIS/RcppParallel thread environment
#' settings are recorded, including unset values, and changes invalidate resume.
#' These are declared environment settings, not measured effective thread counts;
#' backend-specific in-process thread setters still require explicit configuration.
#'
#' A directory lock prevents concurrent execution. After an interrupted process,
#' inspect the lock's owner receipt and verify that process has stopped before
#' manually removing `.lock`; stale locks are never automatically stolen.
#' @param plan A [reflow_imaging_plan()].
#' @param directory New run directory, or existing run directory for resume.
#' @return Invisibly, a named list of stage result objects.
#' @export
reflow_imaging_run <- function(plan, directory) {
  signature <- rf_signature(plan)
  if (file.exists(directory)) stop("Run destination already exists; use resume or a new path.")
  if (!dir.create(directory, recursive = TRUE)) stop("Cannot create run directory.")
  rf_write(signature, file.path(directory, "definition.rds"))
  rf_execute(plan, normalizePath(directory), signature)
}

#' @rdname reflow_imaging_run
#' @export
reflow_imaging_resume <- function(plan, directory) {
  if (!file.exists(file.path(directory, "definition.rds"))) stop("Missing run definition.")
  rf_execute(plan, normalizePath(directory, mustWork = TRUE), NULL)
}

rf_write <- function(x, path) {
  tmp <- tempfile(".receipt-", dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(x, tmp, version = 2)
  if (!file.rename(tmp, path)) stop("Cannot publish receipt: ", path)
}

rf_execute <- function(plan, directory, signature) {
  lock <- file.path(directory, ".lock")
  if (!dir.create(lock, showWarnings = FALSE)) stop("Run locked; inspect .lock/owner.rds.")
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  rf_write(
    list(pid = Sys.getpid(), host = Sys.info()[["nodename"]], time = Sys.time()),
    file.path(lock, "owner.rds")
  )
  ready <- file.path(directory, "READY")
  unlink(ready)
  state_path <- file.path(directory, "state.rds")
  rf_write(list(status = "RUNNING", time = Sys.time()), state_path)
  fail_run <- function(e) {
    unlink(ready)
    rf_write(list(status = "FAILED", error = conditionMessage(e), time = Sys.time()), state_path)
    stop(e)
  }
  tryCatch(
    {
      current <- rf_signature(plan)
      saved <- readRDS(file.path(directory, "definition.rds"))
      if (!identical(current, saved) || (!is.null(signature) && !identical(current, signature))) {
        stop("Plan, input, package or runtime changed; create a new run.")
      }
      values <- list()
      for (s in plan$stages) {
        receipt_path <- file.path(directory, paste0(s$id, ".rds"))
        args <- rf_walk(s$args, function(x) {
          if (inherits(x, "reflow_imaging_input")) {
            return(if (x$read) readRDS(x$path) else x$path)
          }
          value <- values[[x$stage]]
          for (key in x$select) {
            if (is.null(names(value)) || !key %in% names(value)) {
              stop("Missing reference component: ", key)
            }
            value <- value[[key]]
          }
          value
        })
        argument_hash <- rf_hash(args)
        old <- if (file.exists(receipt_path)) readRDS(receipt_path) else NULL
        if (!is.null(old) && !old$status %in% c("READY", "FAILED", "RUNNING")) {
          stop("Invalid stage cache; create a new run: ", s$id)
        }
        if (!is.null(old) && identical(old$status, "READY")) {
          tryCatch(
            {
              if (!identical(old$schema, "reflow_stage_1") || !identical(old$stage, s$id) ||
                    !identical(old$argument_hash, argument_hash) ||
                    !identical(old$config_hash, rf_hash(s)) ||
                    !identical(old$input_hash, current$input_hash) ||
                    !identical(old$package_hash, current$package_hash) ||
                    !identical(old$runtime_hash, current$runtime_hash) ||
                    !identical(old$definition_hash, rf_hash(saved)) ||
                    !is.character(old$output) || length(old$output) != 1L ||
                    !grepl("^attempt-[A-Za-z0-9]+/result[.]rds$", old$output)) {
                stop("Invalid stage receipt.")
              }
              output <- file.path(directory, old$output)
              if (!file.exists(output) || !identical(rf_file_hash(output), old$output_hash)) {
                stop("Corrupt stage output: ", s$id)
              }
              values[s$id] <- list(readRDS(output))
            },
            error = function(e) {
              old$status <- "INVALID"
              old$error <- conditionMessage(e)
              rf_write(old, receipt_path)
              stop(e)
            }
          )
          next
        }
        attempt <- tempfile("attempt-", directory)
        if (!dir.create(attempt)) stop("Cannot create attempt.")
        receipt <- list(
          schema = "reflow_stage_1", stage = s$id, status = "RUNNING",
          definition_hash = rf_hash(saved), config_hash = rf_hash(s),
          input_hash = current$input_hash, argument_hash = argument_hash,
          package_hash = current$package_hash, runtime_hash = current$runtime_hash,
          started = Sys.time()
        )
        save_receipt <- function(x) {
          rf_write(x, file.path(attempt, "receipt.rds"))
          rf_write(x, receipt_path)
        }
        save_receipt(receipt)
        fail_stage <- function(e) {
          receipt$status <- "FAILED"
          receipt$error <- conditionMessage(e)
          save_receipt(receipt)
          stop(e)
        }
        value <- tryCatch(
          {
            value <- rf_call(s, args)
            rf_walk(value, identity)
            output <- file.path(attempt, "result.rds")
            saveRDS(value, output, version = 2)
            # Detect input mutation while a package function was running.
            if (!identical(rf_signature(plan), saved)) {
              stop("Inputs or runtime changed during execution.")
            }
            receipt$status <- "READY"
            receipt$output <- paste0(basename(attempt), "/result.rds")
            receipt$output_hash <- rf_file_hash(output)
            receipt$finished <- Sys.time()
            save_receipt(receipt)
            value
          },
          error = fail_stage,
          interrupt = fail_stage
        )
        values[s$id] <- list(value)
      }
      rf_write(
        list(status = "READY", definition_hash = rf_hash(saved),
             time = Sys.time()), state_path
      )
      writeLines(rf_hash(saved), ready)
      invisible(values)
    },
    error = fail_run,
    interrupt = fail_run
  )
}

rf_call <- function(stage, args) {
  rng <- function() get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  before <- rng()
  kind <- RNGkind()
  on.exit({
    do.call(RNGkind, as.list(kind))
    if (is.null(before)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    } else {
      # Base R requires this exact RNG variable name.
      assign(".Random.seed", before, envir = .GlobalEnv) # nolint: object_name_linter.
    }
  }, add = TRUE)
  if (!is.null(stage$seed)) set.seed(stage$seed)
  value <- do.call(getExportedValue(stage$package, stage$fun), args)
  if (is.null(stage$seed) && !identical(before, rng())) {
    stop("Stochastic stage requires an explicit seed.")
  }
  value
}
