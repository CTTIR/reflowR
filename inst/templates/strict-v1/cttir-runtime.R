.strict_cttir_controls <- c("cttir-study.json", "cttir-recipe.json", "cttir-runtime.json")
.strict_cttir_base_begin <- strict_stage_begin
.strict_cttir_base_finish <- strict_stage_finish
.strict_cttir_check_controls <- function() {
  current <- .strict_runtime_hashes(names(.strict_cttir_expected_controls))
  if (!identical(current, .strict_cttir_expected_controls))
    stop("cttiR controls changed from the compiled declaration")
  current
}
strict_stage_begin <- function(stage, seed) {
  controls <- .strict_cttir_check_controls()
  declaration <- jsonlite::read_json("reflow-profile.json", simplifyVector = FALSE)
  if (!identical(declaration$profile$profile_version, "strict-cttir-v1"))
    stop("The cttiR runtime requires the composed profile")
  runtime <- jsonlite::read_json("cttir-runtime.json", simplifyVector = FALSE)
  checked <- .strict_cttir_runtime_status(runtime)
  if (!isTRUE(checked$current)) stop(checked$reason)
  state <- .strict_cttir_base_begin(stage, seed)
  state$composition_before <- controls
  state
}
strict_stage_finish <- function(state, analysis_file, code_file) {
  current <- .strict_cttir_check_controls()
  if (!identical(state$composition_before, current)) stop("cttiR study, recipe or runtime controls changed during rendering")
  runtime <- jsonlite::read_json("cttir-runtime.json", simplifyVector = FALSE)
  checked <- .strict_cttir_runtime_status(runtime)
  if (!isTRUE(checked$current)) stop(checked$reason)
  receipt <- .strict_cttir_base_finish(state, analysis_file, code_file)
  receipt$code <- c(receipt$code, current[.strict_cttir_controls])
  receipt$composition <- "strict-cttir-v1"
  jsonlite::write_json(receipt, file.path("output/provenance", paste0(state$stage, ".json")),
                      auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(receipt)
}
