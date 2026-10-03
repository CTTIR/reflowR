# Installed data-only artifact worker. No arbitrary source or code arguments.
args <- commandArgs(TRUE)
stopifnot(length(args) == 2L, grepl("^[0-9a-f]{64}$", args[[2]]))
stopifnot(identical(digest::digest(file = args[[1]], algo = "sha256", serialize = FALSE),
  args[[2]]))
request <- readRDS(args[[1]])
# Exact structural validation precedes loading the declared package or writer.
entry <- sub("^--file=", "", commandArgs(FALSE)[grep("^--file=", commandArgs(FALSE))][1])
schema <- file.path(dirname(entry), "schema.R")
stopifnot(
  is.character(request$runtime_sha256),
  schema %in% names(request$runtime_sha256),
  identical(digest::digest(file = schema, algo = "sha256", serialize = FALSE),
    request$runtime_sha256[[schema]])
)
schema_env <- new.env(parent = baseenv())
sys.source(schema, envir = schema_env)
schema_env$rb_request(request)
stopifnot(is.list(request), identical(request$schema, 1L))
.libPaths(request$libraries)
stopifnot(identical(.libPaths(), request$libraries))
for (name in names(request$package_paths)) {
  stopifnot(identical(find.package(name), request$package_paths[[name]]))
  if (isNamespaceLoaded(name)) stopifnot(identical(if (identical(name,
    "base")) file.path(R.home("library"), "base") else getNamespaceInfo(asNamespace(name),
    "path"), request$package_paths[[name]]))
}
stopifnot(identical(find.package("reflowR"), request$package_path))
stopifnot(requireNamespace("reflowR", quietly = TRUE))
stopifnot(identical(getNamespaceInfo(asNamespace("reflowR"), "path"), request$package_path))
for (file in names(request$runtime_sha256)) {
  reflowR:::rfa_no_links(file)
  stopifnot(
    reflowR:::rfa_regular(file),
    identical(
      digest::digest(file = file, algo = "sha256", serialize = FALSE),
      request$runtime_sha256[[file]]
    )
  )
}
env <- new.env(parent = baseenv())
sys.source(request$channel_script, envir = env)
forbidden <- strsplit(Sys.getenv("RFX_FORBIDDEN_FD_INODES"), ",", fixed = TRUE)[[1]]
env$rg_assert_no_inodes(forbidden)
if (!identical(list(locale = Sys.getlocale(), rng_kind = RNGkind()), request$runtime_observation)) {
  stop("Child locale or RNG kind differs from captured parent runtime.")
}
if (!identical(reflowR:::rfa_signature(request$spec), request$expected_definition)) {
  stop("Artifact definition changed since client preflight.")
}
if (isTRUE(request$resume)) {
  reflowR::reflow_artifact_resume(request$spec, request$directory,
    reconciled_attempt = request$reconciled_attempt,
    reconciliation = request$reconciliation
  )
} else {
  reflowR::reflow_artifact_run(request$spec, request$directory)
}
