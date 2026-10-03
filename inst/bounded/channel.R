rg_identity <- function(pid) {
  base <- file.path("/proc", as.character(pid))
  u1 <- file.info(base)$uid
  fields <- strsplit(sub("^.*\\) ", "", readLines(file.path(base, "stat"), warn = FALSE)),
    " +")[[1]]
  u2 <- file.info(base)$uid
  if (length(fields) < 20L || is.na(u1) || is.na(u2) || u1 != u2 || !grepl("^[0-9]+$",
    fields[[20]])) stop("Client identity unknown")
  list(pid = as.integer(pid), uid = u1, start_ticks = fields[[20]])
}
rg_stable_identity <- function(pid) {
  a <- rg_identity(pid)
  b <- rg_identity(pid)
  if (!identical(a, b)) stop("Identity race")
  a
}
rg_state <- function(read, write, token, hash) {
  e <- new.env(parent = emptyenv())
  e$read <- read
  e$write <- write
  e$token <- token
  e$hash <- hash
  e$buffer <- raw()
  e$eof <- FALSE
  e
}
rg_receive <- function(channel, ms = 0L) {
  processx::poll(list(input = channel$read), as.integer(ms))
  b <- processx::conn_read_bytes(channel$read, 4097L)
  channel$buffer <- c(channel$buffer, b)
  if (length(channel$buffer) > 4096L) stop("Oversized channel buffer")
  channel$eof <- !processx::conn_is_incomplete(channel$read)
  lines <- character()
  repeat {
    end <- which(channel$buffer == as.raw(10))
    if (!length(end)) break
    n <- end[[1]]
    if (n > 256L || n == 1L) stop("Invalid frame length")
    frame <- rawToChar(channel$buffer[seq_len(n - 1L)])
    if (!grepl("^[A-Z]+\\t1\\t[0-9a-f]{64}\\t[0-9a-f]{64}$", frame)) stop("Malformed frame")
    fields <- strsplit(frame, "\t", fixed = TRUE)[[1]]
    if (fields[[3]] != channel$token ||
      fields[[4]] != channel$hash) stop("Channel binding mismatch")
    lines <- c(lines, fields[[1]])
    channel$buffer <-
      if (n == length(channel$buffer)) raw() else channel$buffer[(n + 1L):length(channel$buffer)]
  }
  if (channel$eof && length(channel$buffer)) stop("Partial frame at EOF")
  lines
}
rg_send <- function(channel, verb, deadline, alive = function() TRUE) {
  if (!verb %in% c("READY", "START", "CANCEL", "RUNNING", "FINISHED")) stop("Unknown verb")
  pending <- charToRaw(paste0(verb, "\t1\t", channel$token, "\t", channel$hash, "\n"))
  while (length(pending)) {
    if (proc.time()[["elapsed"]] >= deadline || !alive()) stop("Channel write deadline/client loss")
    pending <- processx::conn_write(channel$write, pending)
    if (length(pending)) Sys.sleep(.02)
  }
}
rg_fd_flags <- function(con) {
  path <- sprintf("/proc/self/fdinfo/%d", processx::conn_get_fileno(con))
  x <- readLines(path, warn = FALSE)
  z <- sub("^flags:[[:space:]]*", "", grep("^flags:", x, value = TRUE))
  flags <- strtoi(z, base = 8L)
  if (length(flags) != 1L || is.na(flags) || bitwAnd(flags,
    2048L) == 0L) stop("Channel must be nonblocking")
  list(fd = processx::conn_get_fileno(con), flags = z, inode = sub("^ino:[[:space:]]*", "",
    grep("^ino:", x, value = TRUE)))
}

rg_inode_from_fdinfo <- function(lines) {
  value <- sub("^ino:[[:space:]]*", "", grep("^ino:", lines, value = TRUE))
  if (length(value) != 1L || is.na(value) || !grepl("^[0-9]+$", value)) stop("Unknown fd inode")
  value
}
rg_fd_inventory <- function() {
  files <- list.files("/proc/self/fdinfo", pattern = "^[0-9]+$", full.names = TRUE)
  result <- character()
  for (file in files) {
    lines <- tryCatch(suppressWarnings(readLines(file, warn = FALSE)), error = function(e) NULL)
    if (is.null(lines)) {
      if (!file.exists(file)) next
      stop("Unreadable live file descriptor")
    }
    result[[basename(file)]] <- rg_inode_from_fdinfo(lines)
  }
  result
}
rg_assert_no_inodes <- function(forbidden, inventory = rg_fd_inventory()) {
  if (!is.character(forbidden) || !length(forbidden) || anyNA(forbidden) ||
    !all(grepl("^[0-9]+$", forbidden)) || !is.character(inventory) || anyNA(inventory)) {
    stop("Invalid fd absence evidence")
  }
  if (any(inventory %in% forbidden)) stop("Forbidden channel inode is inherited")
  invisible(TRUE)
}

rg_process_state <- function(pid) {
  first <- rg_identity(pid)
  fields <- strsplit(sub("^.*\\) ", "", readLines(sprintf("/proc/%d/stat", pid), warn = FALSE)),
    " +")[[1]]
  last <- rg_identity(pid)
  if (!identical(first, last) || !length(fields)) stop("Client state identity race")
  list(identity = last, state = fields[[1]])
}
rg_liveness_decision <- function(first, last, expected, handle_running) {
  if (!is.list(first) || !is.list(last) || !identical(first$identity, expected) ||
    !identical(last$identity, expected) || !identical(handle_running, TRUE)) {
    return(FALSE)
  }
  valid <- function(state) {
    is.character(state) && length(state) == 1L && !is.na(state) &&
      state %in% c("R", "S", "D", "T", "t", "I", "W", "P")
  }
  isTRUE(valid(first$state) && valid(last$state))
}
rg_observe_client <- function(expected, handle) {
  first <- NULL
  last <- NULL
  running <- FALSE
  error <- NULL
  tryCatch(
    {
      first <- rg_process_state(expected$pid)
      running <- ps::ps_is_running(handle)
      last <- rg_process_state(expected$pid)
    },
    error = function(e) error <<- conditionMessage(e)
  )
  list(
    alive = is.null(error) && rg_liveness_decision(first, last, expected, running),
    first = first, last = last, handle_running = running, error = error,
    elapsed = proc.time()[["elapsed"]]
  )
}
