#' Generate Analysis Rmd Files
#'
#' Creates individual Rmd files for the analysis pipeline based on the
#' selected depth preset.
#'
#' @param project_dir Character. Path to the project root.
#' @param file_name Character. Base file name (e.g., `"01_read_data"`).
#' @param author Character. Author name.
#' @param description Character. Step description.
#' @param scheme A scheme list from [reflow_scheme()].
#' @param preset A preset list from [reflow_preset()].
#'
#' @return Invisibly returns the output file path.
#'
#' @keywords internal
generate_rmd <- function(project_dir, file_name, author, description,
                         scheme, preset) {
  step_num <- sub("^(\\d+)_.*", "\\1", file_name)
  step_name <- gsub("_", " ", sub("^\\d+_", "", file_name))
  step_title <- paste0(toupper(substring(step_name, 1, 1)),
                       substring(step_name, 2))
  title <- paste0(step_num, " -- ", step_title)

  # Get content based on file name pattern
  content <- get_rmd_content(file_name, scheme, preset)

  rmd <- glue::glue('---
title: "{title}"
author: "{author}"
date: "`r format(Sys.Date(), \'%d %B %Y\')`"
output:
  workflowr::wflow_html:
    toc: true
    code_folding: show
editor_options:
  chunk_output_type: console
---

## Overview

> {description}

```{{r setup, include=FALSE}}
source(here::here("code", "00_setup.R"))
source(here::here("code", "utils.R"))
source(here::here("code", "formulary.R"))
source(here::here("code", "data_io.R"))
source(here::here("code", "table_helpers.R"))
source(here::here("code", "plot_helpers.R"))
```

{content}

---

## Session Info

```{{r session-info}}
sessionInfo()
```
')

  out_path <- fs::path(project_dir, "analysis", paste0(file_name, ".Rmd"))
  writeLines(rmd, out_path)
  invisible(out_path)
}


#' Generate index.Rmd
#'
#' @param project_dir Character. Path to project root.
#' @param name Character. Project name.
#' @param author Character. Author name.
#' @param email Character or NULL. Author email.
#' @param scheme A scheme list.
#' @param preset A preset list.
#'
#' @return Invisibly returns the output file path.
#' @keywords internal
generate_index_rmd <- function(project_dir, name, author, email, scheme, preset) {
  p <- preset

  # Build pipeline table rows
  table_rows <- vapply(p$files, function(f) {
    step_num <- sub("^(\\d+)_.*", "\\1", f)
    step_name <- gsub("_", " ", sub("^\\d+_", "", f))
    step_title <- paste0(toupper(substring(step_name, 1, 1)),
                         substring(step_name, 2))
    desc <- p$descriptions[[f]]
    glue::glue("| [{step_num}]({f}.html) | {step_title} | {desc} |")
  }, character(1))
  table_yaml <- paste(table_rows, collapse = "\n")

  email_line <- if (!is.null(email)) {
    glue::glue("\n**Contact:** [{email}](mailto:{email})\n")
  } else ""

  rmd <- glue::glue('---
title: "{name}"
site: workflowr::wflow_site
output:
  workflowr::wflow_html:
    toc: false
editor_options:
  chunk_output_type: console
---

# {name}

**Author:** {author}
{email_line}
**Scheme:** {scheme$label} | **Pipeline:** {p$label}

---

## Analysis Pipeline

| Step | Name | Description |
|------|------|-------------|
{table_yaml}

---

## Quick Start

1. Place your data in `data/raw/`
2. Open `analysis/01_read_data.Rmd` and update the file path
3. Run `workflowr::wflow_build()` to render all pages
4. Run `workflowr::wflow_publish("analysis/*.Rmd", "Initial build")` to commit

## Project Structure

```
{name}/
+-- analysis/    # Rmd pipeline files (rendered to docs/)
+-- assets/      # Theme CSS, SCSS, and icon
+-- code/        # R scripts (setup, utils, helpers)
+-- data/raw/    # Raw data (never modify)
+-- data/processed/  # Cleaned data
+-- docs/        # Rendered HTML site
+-- output/      # Figures, tables, models
+-- report/      # Quarto report template
```
')

  writeLines(rmd, fs::path(project_dir, "analysis", "index.Rmd"))
  invisible(fs::path(project_dir, "analysis", "index.Rmd"))
}


#' Generate about.Rmd
#'
#' @param project_dir Character. Path to project root.
#' @param name Character. Project name.
#' @param author Character. Author name.
#' @param email Character or NULL. Author email.
#' @param affiliation Character or NULL. Affiliation.
#' @param orcid Character or NULL. ORCID iD.
#' @param website Character or NULL. Website URL.
#' @param scheme A scheme list.
#' @param preset A preset list.
#'
#' @return Invisibly returns the output file path.
#' @keywords internal
generate_about_rmd <- function(project_dir, name, author, email,
                               affiliation, orcid, website, scheme, preset) {
  # Build author card details
  details <- character(0)
  if (!is.null(email)) {
    details <- c(details, glue::glue("- **Email:** [{email}](mailto:{email})"))
  }
  if (!is.null(affiliation)) {
    details <- c(details, glue::glue("- **Affiliation:** {affiliation}"))
  }
  if (!is.null(orcid)) {
    details <- c(details, glue::glue("- **ORCID:** [{orcid}](https://orcid.org/{orcid})"))
  }
  if (!is.null(website)) {
    details <- c(details, glue::glue("- **Website:** [{website}]({website})"))
  }
  details_str <- if (length(details) > 0) paste(details, collapse = "\n") else ""

  pipeline_desc <- vapply(preset$files, function(f) {
    desc <- preset$descriptions[[f]]
    step_num <- sub("^(\\d+)_.*", "\\1", f)
    step_name <- gsub("_", " ", sub("^\\d+_", "", f))
    glue::glue("1. **Step {step_num} -- {step_name}:** {desc}")
  }, character(1))
  pipeline_str <- paste(pipeline_desc, collapse = "\n")

  rmd <- glue::glue('---
title: "About"
output:
  workflowr::wflow_html:
    toc: true
editor_options:
  chunk_output_type: console
---

## About This Project

**{name}** is a reproducible research project built with
[workflowr](https://workflowr.github.io/workflowr/) and themed by
[reflowR](https://github.com/r-heller/reflowR).

## Author

**{author}**

{details_str}

## Pipeline Documentation

This project uses the **{preset$label}** pipeline:

{pipeline_str}

## Theme

- **Scheme:** {scheme$label}
- **Primary color:** `{scheme$primary}`
- **Toolkit:** workflowr + reflowR

## Reproducibility

This site was built with `workflowr::wflow_build()`.
All analysis code is version-controlled and the rendered HTML is in `docs/`.

```{{r session-info}}
sessionInfo()
```
')

  writeLines(rmd, fs::path(project_dir, "analysis", "about.Rmd"))
  invisible(fs::path(project_dir, "analysis", "about.Rmd"))
}


# --- Internal: Get Rmd body content per step ---

get_rmd_content <- function(file_name, scheme, preset) {
  # Match on the step name (without number prefix)
  step <- sub("^\\d+_", "", file_name)

  switch(step,
    read_data = get_content_read_data(),
    tidy = get_content_tidy(),
    eda = get_content_eda(),
    preprocess = get_content_preprocess(),
    quality_control = get_content_quality_control(),
    descriptive = get_content_descriptive(),
    inference = get_content_inference(),
    modelling = get_content_modelling(),
    model_diagnostics = get_content_model_diagnostics(),
    dim_reduction = get_content_dim_reduction(),
    sensitivity = get_content_sensitivity(),
    sub_reports = get_content_sub_reports(),
    report = get_content_report_rmd(),
    final_report = get_content_final_report(),
    # Fallback
    glue::glue('\n## TODO\n\nAdd analysis content for this step.\n')
  )
}


get_content_read_data <- function() {
'
## Data Sources

List your data files and their provenance here.

## Import Data

```{r read-data}
# --- Update the file path to your actual data ---
# Single file
# df <- wf_read_data(file.path(paths$raw, "my_data.csv"))

# Or batch read all files in data/raw/
# data_list <- wf_read_all()
```

## Initial Inspection

```{r inspect-data, eval=FALSE}
# Dimensions
dim(df)

# Structure
str(df)

# First rows
head(df)

# Column types
sapply(df, class)
```

## Data Dictionary

```{r data-dict, eval=FALSE}
dict <- wf_data_dict(df)
dict |> gt::gt() |> gt_wf_style()
```

## Quick Summary

```{r quick-summary, eval=FALSE}
skimr::skim(df)
```

## Save Checkpoint

```{r save-checkpoint, eval=FALSE}
save_versioned(df, "01_raw_imported")
```
'
}


get_content_tidy <- function() {
'
## Load Data

```{r load-data, eval=FALSE}
df <- wf_load_checkpoint("01_raw_imported")
```
## Clean Column Names

```{r clean-names, eval=FALSE}
df <- df |> janitor::clean_names()
```

## Fix Data Types

```{r fix-types, eval=FALSE}
df <- df |>
  dplyr::mutate(
    # Convert character dates to Date
    # date_col = lubridate::ymd(date_col),

    # Convert to factor
    # group = factor(group, levels = c("Control", "Treatment")),

    # Convert to numeric
    # value = as.numeric(value)
  )
```

## Reshape Data

```{r reshape, eval=FALSE}
# Long to wide
# df_wide <- df |> tidyr::pivot_wider(names_from = variable, values_from = value)

# Wide to long
# df_long <- df |> tidyr::pivot_longer(cols = starts_with("measure_"),
#                                       names_to = "measure", values_to = "value")
```

## Validate

```{r validate, eval=FALSE}
# Check for duplicates
df |> janitor::get_dupes()

# Check expected ranges
stopifnot(all(df$age >= 0 & df$age <= 120, na.rm = TRUE))
```

## Save Checkpoint

```{r save-tidy, eval=FALSE}
save_versioned(df, "02_tidied")
```
'
}


get_content_eda <- function() {
'
## Load Data

```{r load-data, eval=FALSE}
df <- wf_load_checkpoint("01_raw_imported")
```

## Missing Data

```{r missing, eval=FALSE}
miss <- wf_missing_assessment(df)
miss$summary_table |> gt::gt() |> gt_wf_style()
miss$plots$vis_miss
```

## Distributions

```{r distributions, eval=FALSE}
wf_distributions(df, type = "both")
```

## Descriptive Statistics

```{r descriptive, eval=FALSE}
numeric_summary(df) |> gt::gt() |> gt_wf_style()
```

## Correlations

```{r correlations, eval=FALSE}
cor_results <- wf_cor_analysis(df)
cor_results$plot
if (nrow(cor_results$high_cors) > 0) {
  cor_results$high_cors |> gt::gt() |> gt_wf_style()
}
```

## Bivariate Plots

```{r bivariate, eval=FALSE}
# wf_boxjitter(df, x = "group", y = "outcome", title = "Outcome by Group")
```

## Table 1

```{r table1, eval=FALSE}
# wf_table_one(df, strata = "group")
```

## Save Checkpoint

```{r save-eda, eval=FALSE}
save_versioned(df, "02_eda_complete")
```
'
}


get_content_preprocess <- function() {
'
## Load Data

```{r load-data, eval=FALSE}
df <- wf_load_checkpoint("02_tidied")
```

## Missing Data Assessment

```{r missing-assessment, eval=FALSE}
miss <- wf_missing_assessment(df)
cat("Complete cases:", miss$pct_complete, "%\\n")
miss$summary_table |> dplyr::filter(pct_missing > 0) |> gt::gt() |> gt_wf_style()
```

## Outlier Detection

```{r outliers, eval=FALSE}
outliers <- wf_detect_outliers(df, method = "iqr", threshold = 1.5)
cat("Total outliers detected:", nrow(outliers), "\\n")
outliers |> dplyr::count(variable) |> gt::gt() |> gt_wf_style()
```

## Imputation

```{r imputation, eval=FALSE}
# Simple median imputation (replace with mice/missForest for production)
# df_imputed <- df |>
#   dplyr::mutate(dplyr::across(where(is.numeric),
#     ~dplyr::if_else(is.na(.x), median(.x, na.rm = TRUE), .x)))
```

## Transformations

```{r transformations, eval=FALSE}
# Log-transform skewed variables
# df <- df |> dplyr::mutate(log_value = log1p(value))

# Standardize
# df <- df |> dplyr::mutate(dplyr::across(where(is.numeric), scale))
```

## Feature Engineering

```{r feature-eng, eval=FALSE}
# Add derived variables
# df <- df |> dplyr::mutate(
#   bmi = weight / (height/100)^2,
#   age_group = cut(age, breaks = c(0, 30, 50, 70, Inf),
#                   labels = c("Young", "Middle", "Senior", "Elderly"))
# )
```

## Save Checkpoint

```{r save-preprocessed, eval=FALSE}
save_versioned(df, "03_preprocessed")
```
'
}


get_content_quality_control <- function() {
'
## Load Data

```{r load-data, eval=FALSE}
df <- wf_load_checkpoint("03_preprocessed")
```

## Completeness Checks

```{r completeness, eval=FALSE}
miss <- wf_missing_assessment(df)
miss$summary_table |> gt::gt() |> gt_wf_style()
cat("Overall completeness:", miss$pct_complete, "%\\n")
```

## Range & Plausibility Checks

```{r range-checks, eval=FALSE}
# Define expected ranges
# ranges <- list(
#   age = c(0, 120),
#   bmi = c(10, 80),
#   systolic_bp = c(60, 250)
# )
#
# purrr::iwalk(ranges, function(rng, var) {
#   out_of_range <- sum(df[[var]] < rng[1] | df[[var]] > rng[2], na.rm = TRUE)
#   cat(var, ":", out_of_range, "values out of range\\n")
# })
```

## Duplicate Detection

```{r duplicates, eval=FALSE}
dupes <- df |> janitor::get_dupes()
cat("Duplicate rows:", nrow(dupes), "\\n")
if (nrow(dupes) > 0) dupes |> head(20) |> gt::gt() |> gt_wf_style()
```

## Cross-Field Validation

```{r cross-field, eval=FALSE}
# Example: discharge date must be after admission date
# invalid <- df |> dplyr::filter(discharge_date < admission_date)
# cat("Invalid date sequences:", nrow(invalid), "\\n")
```

## Data Quality Report

```{r quality-report, eval=FALSE}
# quality_summary <- tibble::tibble(
#   check = c("Completeness", "Duplicates", "Range violations", "Cross-field"),
#   status = c(
#     paste0(miss$pct_complete, "% complete"),
#     paste0(nrow(dupes), " duplicates"),
#     "See above",
#     "See above"
#   )
# )
# quality_summary |> gt::gt() |> gt_wf_style()
```

## Save QC Report

```{r save-qc, eval=FALSE}
save_versioned(df, "04_qc_passed")
```
'
}


get_content_descriptive <- function() {
'
## Load Data

```{r load-data, eval=FALSE}
df <- wf_load_checkpoint("03_preprocessed")
```

## Table 1

```{r table1, eval=FALSE}
# Stratified summary
# tbl1 <- wf_table_one(df, strata = "group")
# tbl1
#
# # Export as gt
# tbl1 |> gtsummary::as_gt() |> gt_wf_style() |>
#   wf_gt_save("table1")
```

## Distributions

```{r distributions, eval=FALSE}
wf_distributions(df, type = "both")
```

## Correlations

```{r correlations, eval=FALSE}
cor_results <- wf_cor_analysis(df)
cor_results$plot

if (nrow(cor_results$high_cors) > 0) {
  cat("Highly correlated pairs (|r| >=", 0.7, "):\\n")
  cor_results$high_cors |> gt::gt() |> gt_wf_style()
}
```

## Missing Data Patterns

```{r missing-patterns, eval=FALSE}
miss <- wf_missing_assessment(df, pattern_plot = TRUE)
miss$plots$vis_miss
```

## Frequency Tables

```{r frequencies, eval=FALSE}
# Categorical variables
# cat_vars <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
# purrr::walk(cat_vars, function(v) {
#   cat("\\n###", v, "\\n")
#   freq_table(df, v) |> print()
# })
```

## Summary Statistics

```{r summary-stats, eval=FALSE}
numeric_summary(df) |> gt::gt() |> gt_wf_style()
```
'
}


get_content_inference <- function() {
'
## Load Data

```{r load-data, eval=FALSE}
df <- wf_load_checkpoint("03_preprocessed")
```

## Normality Assessment

```{r normality, eval=FALSE}
norm_results <- wf_normality_battery(df)
norm_results |> gt::gt() |> gt_wf_style()
```

## Group Comparisons

```{r group-compare, eval=FALSE}
# Two-group comparison
# result <- wf_compare_groups(df, outcome = "value", group = "group")
# cat("Test:", result$test_name, "\\n")
# print(result$test)
# print(result$effect_size)

# Visualise
# wf_boxjitter(df, x = "group", y = "value",
#              title = "Outcome by Group")
```

## Paired Comparisons

```{r paired, eval=FALSE}
# For pre-post designs
# result_paired <- wf_compare_groups(df, outcome = "change",
#                                     group = "time", paired = TRUE)
# wf_paired_plot(df, id = "patient_id", time_var = "time",
#                value_var = "measurement")
```

## Multiple Testing

```{r multiple-testing, eval=FALSE}
# If testing multiple outcomes
# p_values <- c(0.003, 0.04, 0.02, 0.15, 0.001)
# p_adjusted <- p.adjust(p_values, method = "BH")
# tibble::tibble(
#   test = paste0("Test_", seq_along(p_values)),
#   p_raw = p_values,
#   p_adjusted = p_adjusted,
#   significant = p_adjusted < 0.05
# ) |> gt::gt() |> gt_wf_style()
```

## Cross-Tables

```{r crosstabs, eval=FALSE}
# wf_crosstab(df, row_var = "exposure", col_var = "outcome")
```

## Correlation Tests

```{r cor-tests, eval=FALSE}
# cor.test(df$x, df$y, method = "pearson")
# cor.test(df$x, df$y, method = "spearman")
```
'
}


get_content_modelling <- function() {
'
## Load Data

```{r load-data, eval=FALSE}
df <- wf_load_checkpoint("03_preprocessed")
```

## Data Splitting

```{r split, eval=FALSE}
# set.seed(as.integer(format(Sys.Date(), "%Y%m%d")))
# split <- rsample::initial_split(df, prop = 0.8, strata = outcome)
# df_train <- rsample::training(split)
# df_test  <- rsample::testing(split)
# folds    <- rsample::vfold_cv(df_train, v = 10, strata = outcome)
```

## Recipe

```{r recipe, eval=FALSE}
# rec <- recipes::recipe(outcome ~ ., data = df_train) |>
#   recipes::step_normalize(recipes::all_numeric_predictors()) |>
#   recipes::step_dummy(recipes::all_nominal_predictors()) |>
#   recipes::step_nzv(recipes::all_predictors()) |>
#   recipes::step_impute_median(recipes::all_numeric_predictors())
```

## Model Specifications

```{r models, eval=FALSE}
# Logistic regression
# spec_lr <- parsnip::logistic_reg() |>
#   parsnip::set_engine("glm")
#
# # LASSO
# spec_lasso <- parsnip::logistic_reg(penalty = tune::tune(), mixture = 1) |>
#   parsnip::set_engine("glmnet")
#
# # Random forest
# spec_rf <- parsnip::rand_forest(mtry = tune::tune(), trees = 500,
#                                  min_n = tune::tune()) |>
#   parsnip::set_engine("ranger", importance = "impurity") |>
#   parsnip::set_mode("classification")
```

## Workflows

```{r workflows, eval=FALSE}
# wf_lr <- workflows::workflow() |>
#   workflows::add_recipe(rec) |>
#   workflows::add_model(spec_lr)
#
# wf_lasso <- workflows::workflow() |>
#   workflows::add_recipe(rec) |>
#   workflows::add_model(spec_lasso)
#
# wf_rf <- workflows::workflow() |>
#   workflows::add_recipe(rec) |>
#   workflows::add_model(spec_rf)
```

## Tuning

```{r tuning, eval=FALSE}
# results_lasso <- tune::tune_grid(wf_lasso, resamples = folds, grid = 20)
# results_rf    <- tune::tune_grid(wf_rf, resamples = folds, grid = 20)
#
# tune::autoplot(results_lasso)
# tune::autoplot(results_rf)
```

## Fit Best Models

```{r fit-best, eval=FALSE}
# best_lasso <- tune::finalize_workflow(wf_lasso,
#   tune::select_best(results_lasso, metric = "roc_auc"))
# fit_lasso <- parsnip::fit(best_lasso, df_train)
#
# best_rf <- tune::finalize_workflow(wf_rf,
#   tune::select_best(results_rf, metric = "roc_auc"))
# fit_rf <- parsnip::fit(best_rf, df_train)
```

## Test Set Evaluation

```{r evaluate, eval=FALSE}
# preds_lr    <- predict(fit_lr, df_test, type = "prob")
# preds_lasso <- predict(fit_lasso, df_test, type = "prob")
# preds_rf    <- predict(fit_rf, df_test, type = "prob")
```

## Save Models

```{r save-models, eval=FALSE}
# save_versioned(fit_lasso, "06_model_lasso", dir = paths$models)
# save_versioned(fit_rf, "06_model_rf", dir = paths$models)
```
'
}


get_content_model_diagnostics <- function() {
'
## Load Models

```{r load-models, eval=FALSE}
# fit_lr    <- wf_load_checkpoint("06_model_lr", dir = paths$models)
# fit_lasso <- wf_load_checkpoint("06_model_lasso", dir = paths$models)
```

## Residual Analysis

```{r residuals, eval=FALSE}
# For linear models
# augmented <- broom::augment(fit_lr)
#
# ggplot2::ggplot(augmented, ggplot2::aes(.fitted, .resid)) +
#   ggplot2::geom_point(alpha = 0.5, color = wf_colors$primary) +
#   ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
#   ggplot2::geom_smooth(se = FALSE, color = wf_colors$accent) +
#   theme_wf() +
#   ggplot2::labs(title = "Residuals vs Fitted", x = "Fitted", y = "Residuals")
```

## ROC Curves

```{r roc, eval=FALSE}
# roc_lr <- yardstick::roc_curve(preds, truth = outcome, .pred_positive)
# wf_roc_plot(roc_lr)
```

## Calibration

```{r calibration, eval=FALSE}
# wf_calibration_plot(predicted = preds$.pred_positive,
#                     observed = as.numeric(df_test$outcome == "positive"))
```

## Model Comparison

```{r comparison, eval=FALSE}
# metrics <- wf_compare_models(results_lr, results_lasso, results_rf)
# metrics |> gt::gt() |> gt_wf_style()
```

## Variable Importance

```{r importance, eval=FALSE}
# vip::vip(fit_rf |> hardhat::extract_fit_parsnip(),
#          aesthetics = list(fill = wf_colors$primary)) +
#   theme_wf()
```
'
}


get_content_dim_reduction <- function() {
'
## Load Data

```{r load-data, eval=FALSE}
df <- wf_load_checkpoint("03_preprocessed")
```

## PCA

```{r pca, eval=FALSE}
# num_vars <- df |> dplyr::select(where(is.numeric)) |> na.omit()
# pca_res <- FactoMineR::PCA(num_vars, graph = FALSE)
#
# # Scree plot
# factoextra::fviz_screeplot(pca_res, addlabels = TRUE) + theme_wf()
#
# # Biplot
# factoextra::fviz_pca_biplot(pca_res, repel = TRUE,
#   col.var = wf_colors$accent, col.ind = wf_colors$primary) + theme_wf()
#
# # Contributions
# factoextra::fviz_contrib(pca_res, choice = "var", axes = 1) + theme_wf()
```

## UMAP

```{r umap, eval=FALSE}
# umap_res <- umap::umap(num_vars)
# umap_df <- tibble::tibble(
#   UMAP1 = umap_res$layout[, 1],
#   UMAP2 = umap_res$layout[, 2]
# )
#
# ggplot2::ggplot(umap_df, ggplot2::aes(UMAP1, UMAP2)) +
#   ggplot2::geom_point(alpha = 0.6, color = wf_colors$primary) +
#   theme_wf() +
#   ggplot2::labs(title = "UMAP Projection")
```

## t-SNE

```{r tsne, eval=FALSE}
# tsne_res <- Rtsne::Rtsne(as.matrix(num_vars), perplexity = 30)
# tsne_df <- tibble::tibble(
#   tSNE1 = tsne_res$Y[, 1],
#   tSNE2 = tsne_res$Y[, 2]
# )
#
# ggplot2::ggplot(tsne_df, ggplot2::aes(tSNE1, tSNE2)) +
#   ggplot2::geom_point(alpha = 0.6, color = wf_colors$primary) +
#   theme_wf() +
#   ggplot2::labs(title = "t-SNE Projection")
```

## Clustering

```{r clustering, eval=FALSE}
# # Optimal clusters
# factoextra::fviz_nbclust(num_vars, stats::kmeans, method = "silhouette") + theme_wf()
#
# km <- stats::kmeans(scale(num_vars), centers = 3, nstart = 25)
# factoextra::fviz_cluster(km, data = num_vars) + theme_wf()
```
'
}


get_content_sensitivity <- function() {
'
## Load Data

```{r load-data, eval=FALSE}
df <- wf_load_checkpoint("03_preprocessed")
```

## Complete Case Analysis

```{r complete-case, eval=FALSE}
# df_cc <- df |> tidyr::drop_na()
# cat("Complete cases:", nrow(df_cc), "of", nrow(df),
#     "(", round(nrow(df_cc)/nrow(df)*100, 1), "%)\\n")
#
# # Re-run primary analysis on complete cases
# # result_cc <- wf_compare_groups(df_cc, outcome = "value", group = "group")
```

## Per-Protocol Analysis

```{r per-protocol, eval=FALSE}
# df_pp <- df |> dplyr::filter(compliant == TRUE)
# cat("Per-protocol:", nrow(df_pp), "of", nrow(df), "\\n")
#
# # Re-run primary analysis on per-protocol set
# # result_pp <- wf_compare_groups(df_pp, outcome = "value", group = "group")
```

## Worst-Case Imputation

```{r worst-case, eval=FALSE}
# Assign worst outcome to missing in treatment, best to control
# df_wc <- df |>
#   dplyr::mutate(value = dplyr::case_when(
#     is.na(value) & group == "Treatment" ~ max(value, na.rm = TRUE),
#     is.na(value) & group == "Control"   ~ min(value, na.rm = TRUE),
#     TRUE ~ value
#   ))
#
# # result_wc <- wf_compare_groups(df_wc, outcome = "value", group = "group")
```

## Comparison of Approaches

```{r comparison, eval=FALSE}
# sensitivity_results <- tibble::tibble(
#   Analysis = c("Primary (ITT)", "Complete Case", "Per-Protocol", "Worst-Case"),
#   Estimate = c(NA, NA, NA, NA),
#   CI_Lower = c(NA, NA, NA, NA),
#   CI_Upper = c(NA, NA, NA, NA),
#   P_Value  = c(NA, NA, NA, NA)
# )
# sensitivity_results |> gt::gt() |> gt_wf_style()
#
# # Forest plot of sensitivity analyses
# # wf_forest_plot(
# #   estimates = sensitivity_results$Estimate,
# #   ci_lower  = sensitivity_results$CI_Lower,
# #   ci_upper  = sensitivity_results$CI_Upper,
# #   labels    = sensitivity_results$Analysis,
# #   title     = "Sensitivity Analyses"
# # )
```
'
}


get_content_sub_reports <- function() {
'
## Load Data

```{r load-data, eval=FALSE}
df <- wf_load_checkpoint("03_preprocessed")
```

## Define Subgroups

```{r subgroups, eval=FALSE}
# subgroups <- list(
#   "Male"    = df |> dplyr::filter(sex == "Male"),
#   "Female"  = df |> dplyr::filter(sex == "Female"),
#   "Age < 65" = df |> dplyr::filter(age < 65),
#   "Age >= 65" = df |> dplyr::filter(age >= 65)
# )
```

## Subgroup Analyses

```{r subgroup-loop, eval=FALSE}
# results <- purrr::map_dfr(names(subgroups), function(sg) {
#   sub_df <- subgroups[[sg]]
#   # Run analysis for each subgroup
#   # result <- wf_compare_groups(sub_df, "value", "group")
#   tibble::tibble(
#     subgroup = sg,
#     n = nrow(sub_df)
#     # estimate = ..., ci_lower = ..., ci_upper = ..., p_value = ...
#   )
# })
#
# results |> gt::gt() |> gt_wf_style()
```

## Interaction Tests

```{r interactions, eval=FALSE}
# Test for subgroup x treatment interaction
# model_int <- lm(value ~ group * sex, data = df)
# broom::tidy(model_int) |> gt::gt() |> gt_wf_style()
```

## Subgroup Forest Plot

```{r forest, eval=FALSE}
# wf_forest_plot(
#   estimates = results$estimate,
#   ci_lower  = results$ci_lower,
#   ci_upper  = results$ci_upper,
#   labels    = results$subgroup,
#   title     = "Subgroup Analysis"
# )
```

## Parameterized Reports

```{r parameterized, eval=FALSE}
# Generate separate reports per subgroup
# purrr::walk(names(subgroups), function(sg) {
#   rmarkdown::render(
#     here::here("report", "report.qmd"),
#     params = list(subgroup = sg),
#     output_file = paste0("report_", janitor::make_clean_names(sg), ".html"),
#     output_dir = paths$output
#   )
# })
```
'
}


get_content_report_rmd <- function() {
'
## Summary

```{r summary, eval=FALSE}
# Load key results
# df <- wf_load_checkpoint("03_preprocessed")
```

## Key Findings

```{r findings, eval=FALSE}
# Summarise main results here
```

## Export

```{r export, eval=FALSE}
# Export final tables and figures
# wf_export(tbl1, "table1_final", formats = c("html", "docx"))
# wf_export(main_plot, "figure1_final")
```
'
}


get_content_final_report <- function() {
'
## Executive Summary

This document integrates all results from the analysis pipeline.

## Methods Summary

```{r methods, eval=FALSE}
# Describe the analysis pipeline and key decisions
```

## Results

### Primary Analysis

```{r primary, eval=FALSE}
# Load and display primary analysis results
```

### Secondary Analyses

```{r secondary, eval=FALSE}
# Load and display secondary analyses
```

### Sensitivity Analyses

```{r sensitivity, eval=FALSE}
# Load and display sensitivity analysis results
```

## Key Tables

```{r key-tables, eval=FALSE}
# Display or load key tables
```

## Key Figures

```{r key-figures, eval=FALSE}
# Display or load key figures
```

## Conclusions

```{r conclusions, eval=FALSE}
# Summarise conclusions
```

## Export Final Report

```{r export, eval=FALSE}
# Optionally render the Quarto report
# quarto::quarto_render(here::here("report", "report.qmd"))
```
'
}
