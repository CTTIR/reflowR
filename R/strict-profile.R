.strict_stage_ids <- c("protocol", "import", "validation", "cleaning", "descriptive",
                       "primary", "diagnostics", "sensitivity", "exploratory",
                       "report", "reproducibility")

strict_text <- function(x, label, maximum = 500L) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x)) ||
      nchar(x, type = "chars") > maximum || grepl("[`{}<>\\\\$\r\n\t]", x)) {
    stop(label, " must be a bounded, nonempty plain-text scalar", call. = FALSE)
  }
  enc2utf8(x)
}

strict_enum <- function(x, values, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !x %in% values)
    stop("Unknown ", label, call. = FALSE)
  x
}

strict_names <- function(x, expected, label) {
  if (!is.list(x) || is.null(names(x)) || anyDuplicated(names(x)) ||
      !setequal(names(x), expected)) stop("Invalid fields in ", label, call. = FALSE)
  invisible(TRUE)
}

strict_json <- function(x) {
  paste0(as.character(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE,
                                     null = "null", digits = NA)), "\n")
}

strict_sha <- function(x) digest::digest(charToRaw(enc2utf8(x)), algo = "sha256", serialize = FALSE)

#' Declare a Strict Research Workflow Profile
#'
#' Creates a versioned declaration for a research-group project. Domain, data
#' origin and design are independent dimensions. A profile records applicable
#' stages; it does not select statistical methods or approve data access.
#'
#' @param domain One of `clinical`, `basic`, `methods`, `software`, or `other`.
#' @param data_origin One of `primary` or `secondary`.
#' @param design One of `experimental`, `observational`, `synthesis`,
#'   `simulation`, `software`, or `other`.
#' @param seed One nonnegative integer for reproducible analysis sessions.
#' @param stages Optional named list of stage overrides. Each value has exactly
#'   `applicable` (logical) and `reason` (plain text) fields. An inapplicable stage
#'   requires a nonempty reason. Unspecified stages remain applicable.
#' @param profile_version `strict-v1` (default), or the explicit cttiR
#'   composition `strict-cttir-v1`.
#' @param data_access One of `unknown`, `public`, `controlled`, `restricted`, or
#'   `synthetic`. This records a declaration, not access authorization.
#' @param jurisdiction Plain-text jurisdiction or `unknown`.
#' @param legal_basis Plain-text access/use basis or `unknown`.
#'
#' @return A declarative list of class `reflow_strict_profile`.
#' @export
#' @examples
#' p <- reflow_profile("clinical", "secondary", "observational")
#' p$scheme
#' p$badge
reflow_profile <- function(domain, data_origin, design, seed = 1L, stages = NULL,
                           profile_version = "strict-v1", data_access = "unknown",
                           jurisdiction = "unknown", legal_basis = "unknown") {
  profile_version <- strict_enum(profile_version, c("strict-v1", "strict-cttir-v1"), "profile version")
  domain <- strict_enum(domain, c("clinical", "basic", "methods", "software", "other"), "domain")
  data_origin <- strict_enum(data_origin, c("primary", "secondary"), "data origin")
  design <- strict_enum(design, c("experimental", "observational", "synthesis", "simulation", "software", "other"), "design")
  data_access <- strict_enum(data_access, c("unknown", "public", "controlled", "restricted", "synthetic"), "data access")
  jurisdiction <- strict_text(jurisdiction, "jurisdiction")
  legal_basis <- strict_text(legal_basis, "legal basis")
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) || !is.finite(seed) ||
      seed < 0 || seed > .Machine$integer.max || seed != as.integer(seed))
    stop("seed must be one nonnegative integer", call. = FALSE)
  if (!is.null(stages) && (!is.list(stages) || is.null(names(stages)) ||
      anyDuplicated(names(stages)) || any(!names(stages) %in% .strict_stage_ids)))
    stop("Unknown or duplicate stage override", call. = FALSE)
  stage_rows <- lapply(seq_along(.strict_stage_ids), function(i) {
    id <- .strict_stage_ids[[i]]
    applicable <- TRUE
    reason <- ""
    if (!is.null(stages[[id]])) {
      entry <- stages[[id]]
      strict_names(entry, c("applicable", "reason"), paste("stage", id))
      if (!is.logical(entry$applicable) || length(entry$applicable) != 1L || is.na(entry$applicable))
        stop("Stage applicability must be one logical value", call. = FALSE)
      applicable <- entry$applicable
      reason <- if (identical(entry$reason, "") && applicable) "" else strict_text(entry$reason, "stage reason")
    }
    stem <- sprintf("%02d_%s", i, id)
    list(id = id, applicable = applicable, reason = reason,
         analysis = paste0("analysis/", stem, ".Rmd"), code = paste0("code/stages/", stem, ".R"))
  })
  scheme <- if (design == "synthesis") "special" else if (
    design %in% c("simulation", "software") || domain %in% c("methods", "software")) "code" else domain
  result <- list(profile_version = profile_version, domain = domain,
                 data_origin = data_origin, design = design, seed = as.integer(seed),
                 data_access = data_access, jurisdiction = jurisdiction,
                 legal_basis = legal_basis, scheme = scheme,
                 primary_color = reflow_scheme(scheme)$primary,
                 badge = paste(toupper(data_origin), "DATA"), stages = stage_rows)
  class(result) <- "reflow_strict_profile"
  result
}

strict_profile_canonical <- function(profile) {
  keys <- c("profile_version", "domain", "data_origin", "design", "seed", "data_access",
            "jurisdiction", "legal_basis", "scheme", "primary_color", "badge", "stages")
  strict_names(profile, keys, "profile")
  if (!is.list(profile$stages) || length(profile$stages) != length(.strict_stage_ids))
    stop("Profile must declare every common stage", call. = FALSE)
  overrides <- lapply(profile$stages, function(s) {
    strict_names(s, c("id", "applicable", "reason", "analysis", "code"), "profile stage")
    list(applicable = s$applicable, reason = s$reason)
  })
  names(overrides) <- vapply(profile$stages, `[[`, character(1), "id")
  canonical <- reflow_profile(profile$domain, profile$data_origin, profile$design,
                              profile$seed, overrides, profile$profile_version,
                              profile$data_access, profile$jurisdiction, profile$legal_basis)
  if (!identical(strict_json(unclass(profile)), strict_json(unclass(canonical))))
    stop("Profile differs from its canonical version or paths", call. = FALSE)
  canonical
}

strict_dependency_lock <- function(lock) {
  if (!is.list(lock) || is.null(names(lock)) || anyDuplicated(names(lock)) ||
      !all(c("R", "Packages") %in% names(lock)) ||
      any(!names(lock) %in% c("R", "Packages", "Bioconductor")))
    stop("dependency_lock must be a parsed R renv lockfile", call. = FALSE)
  if (!is.list(lock$R) || !is.character(lock$R$Version) || length(lock$R$Version) != 1L ||
      !grepl("^[0-9]+\\.[0-9]+(\\.[0-9]+)?$", lock$R$Version))
    stop("Dependency lock must pin R Version", call. = FALSE)
  packages <- lock$Packages
  if (!is.list(packages) || is.null(names(packages)) || anyDuplicated(names(packages)) ||
      !all(c("workflowr", "rmarkdown", "knitr", "digest", "jsonlite") %in% names(packages)))
    stop("Dependency lock lacks required rendering/runtime packages", call. = FALSE)
  for (name in names(packages)) {
    record <- packages[[name]]
    if (!grepl("^[A-Za-z][A-Za-z0-9.]*$", name) || !is.list(record) ||
        !identical(record$Package, name) || !is.character(record$Version) ||
        length(record$Version) != 1L || !grepl("^[0-9][A-Za-z0-9.+-]*$", record$Version))
      stop("Dependency records must pin matching package names and versions", call. = FALSE)
    if (identical(record$Source, "GitHub") &&
        (!is.character(record$RemoteSha) || length(record$RemoteSha) != 1L ||
         !grepl("^[a-fA-F0-9]{40}$", record$RemoteSha)))
      stop("GitHub dependencies require a full commit SHA", call. = FALSE)
  }
  lock$Packages <- packages[sort(names(packages))]
  lock
}
