#' Get a Depth Preset Definition
#'
#' Returns the full preset list for a given preset name.
#'
#' @param name Character. One of `"minimal"`, `"standard"`, `"extended"`.
#'
#' @return A named list containing the preset name, label, file names,
#'   and descriptions.
#'
#' @examples
#' p <- reflow_preset("standard")
#' p$files
#' p$descriptions
#'
#' @seealso [reflow_presets()] for listing all available presets.
#' @export
reflow_preset <- function(name) {
  name <- match.arg(name, names(.presets))
  .presets[[name]]
}

# Internal preset definitions
.presets <- list(
  minimal = list(
    name  = "minimal",
    label = "Minimal (3 steps)",
    files = c("01_read_data", "02_eda", "03_report"),
    descriptions = list(
      "01_read_data" = "Import raw data from data/raw/",
      "02_eda"       = "Exploratory data analysis: summaries, distributions, correlations",
      "03_report"    = "Final summary report and export"
    )
  ),
  standard = list(
    name  = "standard",
    label = "Standard (8 steps)",
    files = c("01_read_data", "02_tidy", "03_preprocess",
              "04_descriptive", "05_inference", "06_modelling",
              "07_dim_reduction", "08_sub_reports"),
    descriptions = list(
      "01_read_data"     = "Import raw data from data/raw/",
      "02_tidy"          = "Clean, reshape, and validate data",
      "03_preprocess"    = "Handle missing data, outliers, transformations",
      "04_descriptive"   = "Table 1, distributions, correlations",
      "05_inference"     = "Hypothesis testing and group comparisons",
      "06_modelling"     = "Predictive modelling with tidymodels",
      "07_dim_reduction" = "PCA, UMAP, t-SNE projections",
      "08_sub_reports"   = "Subgroup analyses and parameterized reports"
    )
  ),
  extended = list(
    name  = "extended",
    label = "Extended (12 steps)",
    files = c("01_read_data", "02_tidy", "03_preprocess",
              "04_quality_control", "05_descriptive", "06_inference",
              "07_modelling", "08_model_diagnostics", "09_dim_reduction",
              "10_sensitivity", "11_sub_reports", "12_final_report"),
    descriptions = list(
      "01_read_data"       = "Import raw data from data/raw/",
      "02_tidy"            = "Clean, reshape, and validate data",
      "03_preprocess"      = "Handle missing data, outliers, transformations",
      "04_quality_control" = "Data quality checks, completeness, plausibility",
      "05_descriptive"     = "Table 1, distributions, correlations, missingness",
      "06_inference"       = "Hypothesis testing and group comparisons",
      "07_modelling"       = "Predictive modelling with tidymodels",
      "08_model_diagnostics" = "Residuals, calibration, ROC/AUC, model comparison",
      "09_dim_reduction"   = "PCA, UMAP, t-SNE projections",
      "10_sensitivity"     = "Sensitivity analyses: complete case, per-protocol, worst-case",
      "11_sub_reports"     = "Subgroup analyses and parameterized reports",
      "12_final_report"    = "Integrated final report with all results"
    )
  )
)
