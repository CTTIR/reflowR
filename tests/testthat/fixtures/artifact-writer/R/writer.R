write_bundle <- function(path, mode = "ordinary", input = NULL) {
  dir.create(path)
  writeLines("first", file.path(path, "first.txt"))
  if (mode == "partial")
    stop("deliberate partial failure")
  if (mode == "interrupt") {
    stop(
      structure(
        list(message = "interrupted", call = NULL),
        class = c("interrupt", "condition")
      )
    )
  }
  if (mode == "random")
    writeLines(
      as.character(stats::runif(1)),
      file.path(path, "first.txt")
    )
  if (mode == "extra")
    dir.create(file.path(path, "unexpected"))
  if (mode == "mutate")
    writeLines("changed input", input)
  if (mode == "slow")
    Sys.sleep(2)
  invisible(new.env())
}
