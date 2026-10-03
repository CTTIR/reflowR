selector_fixture <- function(frame = parent.frame()) {
  skip_if_not(identical(Sys.info()[["sysname"]], "Linux"))
  root <- tempfile("selection ", normalizePath(tempdir(), winslash = "/"))
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE), envir = frame)
  input <- file.path(root, "input.rds")
  saveRDS(c("literal", "second"), input)
  make <- function() {
    reflow_artifact_plan(
    reflow_artifact_stage("producer", "base", "saveRDS",
      list(object = reflow_imaging_input(input, read = TRUE)),
      outputs = list(file = list(path = "data.rds", type = "file")),
      inventory = c(data = "data.rds")),
    reflow_artifact_stage("consumer", "base", "writeLines",
      list(text = reflow_artifact_ref("producer", "data", read = TRUE)),
      outputs = list(con = list(path = "value.txt", type = "file")),
      inventory = c(value = "value.txt")),
    reflow_artifact_stage("independent", "base", "writeLines",
      list(text = "unchanged"),
      outputs = list(con = list(path = "value.txt", type = "file")),
      inventory = c(value = "value.txt")))
  }
  list(root = root, input = input, registry = file.path(root, "registry"), make = make)
}

selector_snapshot <- function(path) {
  files <- list.files(path, recursive = TRUE, full.names = TRUE,
                       all.files = TRUE, no.. = TRUE)
  files <- files[!dir.exists(files)]
  stats::setNames(vapply(files, rf_file_hash, character(1)),
                  substring(files, nchar(path) + 2L))
}

selector_retry <- function(registry, action = "publish_complete") {
  state <- rfs_read(registry)
  list(pending_hash = state$pending_hash, generation = state$active$generation,
       action = action, quiescent = TRUE, reason = "Owned anonymous invocation ended",
       reconciliations = list())
}
