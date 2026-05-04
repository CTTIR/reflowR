#' Generate Quarto Report Template
#'
#' Creates a `report/report.qmd` file with dual HTML + PDF output
#' using the project's SCSS theme.
#'
#' @param project_dir Character. Path to the project root.
#' @param name Character. Project name.
#' @param author Character. Author name.
#' @param scheme A scheme list from [reflow_scheme()].
#' @param preset A preset list from [reflow_preset()].
#'
#' @return Invisibly returns the output file path.
#'
#' @keywords internal
generate_report_qmd <- function(project_dir, name, author, scheme, preset) {
  qmd <- glue::glue('---
title: "{name} -- Report"
author: "{author}"
date: today
date-format: "DD MMMM YYYY"
params:
  subgroup: "all"
format:
  html:
    theme: [flatly, ../assets/_theme.scss]
    toc: true
    toc-depth: 3
    number-sections: true
    code-fold: true
    self-contained: true
    fig-width: 8
    fig-height: 5
  pdf:
    documentclass: article
    geometry: margin=2.5cm
    fontsize: 11pt
    toc: true
    number-sections: true
    fig-width: 7
    fig-height: 4
execute:
  echo: false
  warning: false
  message: false
---

# Introduction

This report summarises the analysis for **{name}**.

```{{r setup}}
#| include: false
source(here::here("code", "00_setup.R"))
source(here::here("code", "utils.R"))
source(here::here("code", "formulary.R"))
source(here::here("code", "data_io.R"))
source(here::here("code", "table_helpers.R"))
source(here::here("code", "plot_helpers.R"))

# Filter by subgroup if specified
subgroup <- params$subgroup
cat("Subgroup:", subgroup, "\\n")
```

## Subgroup

```{{r subgroup-info}}
if (subgroup != "all") {{
  cat("This report is filtered to subgroup:", subgroup)
}}
```

# Methods

Describe the analysis methods here.

# Results

## Descriptive Statistics

```{{r descriptive}}
#| eval: false
# df <- wf_load_checkpoint("03_preprocessed")
# wf_table_one(df) |> gtsummary::as_gt() |> gt_wf_style()
```

## Primary Analysis

```{{r primary}}
#| eval: false
# Display primary analysis results
```

## Figures

```{{r figures}}
#| eval: false
# Display key figures
```

# Discussion

Discuss the findings here.

# Session Info

```{{r session-info}}
sessionInfo()
```
')

  writeLines(qmd, fs::path(project_dir, "report", "report.qmd"))
  invisible(fs::path(project_dir, "report", "report.qmd"))
}


#' Generate Project README
#'
#' Creates a README.md in the project root.
#'
#' @param project_dir Character. Path to the project root.
#' @param name Character. Project name.
#' @param author Character. Author name.
#' @param scheme A scheme list.
#' @param preset A preset list.
#'
#' @return Invisibly returns the output file path.
#' @keywords internal
generate_project_readme <- function(project_dir, name, author, scheme, preset) {
  readme <- glue::glue('# {name}

**Author:** {author}
**Scheme:** {scheme$label}
**Pipeline:** {preset$label}
**Created:** {format(Sys.Date(), "%d %B %Y")}

---

## Project Structure

```
{name}/
+-- analysis/    # Rmd pipeline files
+-- assets/      # Theme CSS, SCSS, icon
+-- code/        # R scripts (setup, utils, helpers)
+-- data/raw/    # Raw data (never modify)
+-- data/processed/  # Cleaned data
+-- docs/        # Rendered HTML site
+-- output/      # Figures, tables, models
+-- report/      # Quarto report template
```

## Getting Started

1. Place data in `data/raw/`
2. Open `analysis/01_read_data.Rmd`
3. Run `workflowr::wflow_build()` to render
4. Run `workflowr::wflow_publish("analysis/*.Rmd", "Build")` to commit

## Built With

- [workflowr](https://workflowr.github.io/workflowr/) -- Reproducible research framework
- [reflowR](https://github.com/r-heller/reflowR) -- Themed workflowr extensions
')

  writeLines(readme, fs::path(project_dir, "README.md"))
  invisible(fs::path(project_dir, "README.md"))
}
