write_value <- function(path, value = "alpha", input = NULL, delay = 0,
                        fail_first = FALSE, object = NULL) {
  if (delay > 0) Sys.sleep(delay)
  if (fail_first) {
    run <- dirname(dirname(dirname(path)))
    attempts <- list.files(run, pattern = "^attempt-")
    if (length(attempts) == 1L) stop("First attempt fails deliberately.")
  }
  if (!is.null(input)) value <- paste(readLines(input, warn = FALSE), value)
  if (!is.null(object)) value <- paste(c(object, value), collapse = " ")
  writeLines(value, path)
  invisible(new.env())
}

write_object <- function(path) saveRDS(c("red", "blue"), path)
