# Load only after installed source pin verification.
rg_client_start <- function(declaration, directory, guardian_script, rscript, channel_script) {
  rfx_path(directory, exists = FALSE)
  if (file.exists(directory) || !dir.create(directory)) stop("Fresh guardian directory required")
  control <- processx::conn_create_pipepair(nonblocking = c(TRUE, TRUE))
  status <- processx::conn_create_pipepair(nonblocking = c(TRUE, TRUE))
  on_error <- TRUE
  on.exit(if (on_error) for (x in c(control, status)) try(processx::processx_conn_close(x),
    silent = TRUE), add = TRUE)
  token <- digest::digest(list(Sys.getpid(), Sys.time(), tempfile()), algo = "sha256")
  declaration$client <- rg_stable_identity(Sys.getpid())
  declaration$client_time <- ps::ps_create_time(ps::ps_handle())
  declaration$token <- token
  declaration$directory <- directory
  declaration$client_writer_inode <- rg_fd_flags(control[[1]])$inode
  declaration$control_read_inode <- rg_fd_flags(control[[2]])$inode
  declaration$status_write_inode <- rg_fd_flags(status[[1]])$inode
  declaration$channel_script <- channel_script
  declaration$guardian_script <- guardian_script
  declaration$source_sha256 <- declaration$all_source_sha256
  request_path <- file.path(directory, "request.rds")
  rfx_new_rds(declaration$request, request_path)
  request_hash <- digest::digest(file = request_path, algo = "sha256", serialize = FALSE)
  declaration$args <- c(declaration$args, request_hash)
  declaration$request <- NULL
  file <- file.path(directory, "declaration.rds")
  rfx_new_rds(declaration, file)
  hash <- digest::digest(file = file, algo = "sha256", serialize = FALSE)
  nice_plan <- rfx_nice_plan(ps::ps_get_nice(ps::ps_handle()), declaration$nice)
  guardian_args <- c(paste0("--as=", format(declaration$address_space_bytes, scientific = FALSE,
    trim = TRUE)), "--core=0", "--", declaration$nice_command, "-n",
    as.character(nice_plan$increment), rscript, "--vanilla", guardian_script, file, hash, "3", "4")
  guardian <- processx::process$new(declaration$prlimit, guardian_args,
    env = c("current", declaration$environment, TMPDIR = declaration$temp_directory,
      TMP = declaration$temp_directory, TEMP = declaration$temp_directory,
      XDG_CACHE_HOME = declaration$cache_directory,
      OPENBLAS_NUM_THREADS = as.character(declaration$threads),
      OMP_NUM_THREADS = as.character(declaration$threads),
      MKL_NUM_THREADS = as.character(declaration$threads)),
    connections = list(control[[2]], status[[1]]), stdin = NULL,
    stdout = file.path(directory, "guardian.out"), stderr = file.path(directory, "guardian.err"),
    cleanup = FALSE, cleanup_tree = FALSE, supervise = FALSE, linux_pdeathsig = FALSE
  )
  processx::processx_conn_close(control[[2]])
  processx::processx_conn_close(status[[1]])
  on_error <- FALSE
  list(
    process = guardian, channel = rg_state(status[[2]], control[[1]], token, hash),
    declaration = declaration, directory = directory,
      client_writer_fd = processx::conn_get_fileno(control[[1]])
  )
}
rg_client_wait <- function(client, verb, seconds = 5) {
  deadline <- proc.time()[["elapsed"]] + seconds
  repeat {
    got <- rg_receive(client$channel, 50L)
    if (length(got)) {
      if (!identical(got, verb)) stop("Unexpected guardian message")
      return(invisible(TRUE))
    }
    if (client$channel$eof || !client$process$is_alive() ||
      proc.time()[["elapsed"]] >= deadline) stop("Guardian unavailable or deadline")
  }
}
