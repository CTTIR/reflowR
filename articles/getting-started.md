# Getting Started with reflowR

[![R-CMD-check](https://github.com/CTTIR/reflowR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CTTIR/reflowR/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/CTTIR/reflowR/actions/workflows/pkgdown.yaml/badge.svg)](https://cttir.github.io/reflowR/)
[![CRAN
status](https://www.r-pkg.org/badges/version/reflowR)](https://CRAN.R-project.org/package=reflowR)
[![Codecov test
coverage](https://codecov.io/gh/CTTIR/reflowR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/CTTIR/reflowR?branch=main)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/reflowR)](https://cran.r-project.org/package=reflowR)
[![CRAN downloads
total](https://cranlogs.r-pkg.org/badges/grand-total/reflowR)](https://cran.r-project.org/package=reflowR)
[![License:
MIT](https://img.shields.io/badge/license-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

### What is reflowR?

**reflowR** extends the
[workflowr](https://workflowr.github.io/workflowr/) package with themed
color schemes, structured analysis pipeline templates, and utility
scripts for reproducible research. It wraps
[`workflowr::wflow_start()`](https://workflowr.github.io/workflowr/reference/wflow_start.html)
and layers on additional structure and theming.

### Installation

``` r

# Install from GitHub (workflowr is installed automatically)
remotes::install_github("cttir/reflowR")
```

### Quick Start

``` r

library(reflowR)

# Create a clinical research project with the standard 8-step pipeline
reflow_init(
  directory = "~/projects/sepsis_trial",
  author    = "Raban Heller",
  email     = "raban.heller@outlook.com",
  scheme    = "clinical",
  depth     = "standard"
)
```

### Choosing a Scheme

reflowR provides five color schemes:

| Scheme | Label | Primary Color | Best For |
|----|----|----|----|
| `clinical` | Clinical Research | Red (#C8102E) | Clinical trials, patient data |
| `basic` | Basic Research | Steelblue (#4682B4) | Lab research, general science |
| `code` | Code & Statistics | Forest Green (#228B22) | Software dev, statistical methods |
| `special` | Special | Purple (#6A0DAD) | Special projects, reviews |
| `other` | Other | Grey (#2C2C2C) | Neutral, multipurpose |

Preview any scheme:

``` r

reflow_preview("clinical")
```

### Choosing a Depth

| Preset     | Steps | Best For                                 |
|------------|-------|------------------------------------------|
| `minimal`  | 3     | Quick explorations, side analyses        |
| `standard` | 8     | Standard research projects, publications |
| `extended` | 12    | Clinical trials, comprehensive studies   |

### Project Structure

After
[`reflow_init()`](https://cttir.github.io/reflowR/reference/reflow_init.md),
your project contains:

    my_project/
    +-- analysis/        # Rmd pipeline files (rendered to docs/)
    |   +-- _site.yml    # Themed navbar + output config
    |   +-- index.Rmd    # Home page with pipeline table
    |   +-- about.Rmd    # Author card + pipeline docs
    |   +-- 01_read_data.Rmd  # ... pipeline steps
    |   +-- ...
    +-- assets/          # Theme CSS, SCSS, and SVG icon
    +-- code/            # R scripts
    |   +-- 00_setup.R   # Packages, paths, colors, ggplot theme
    |   +-- utils.R      # Helper functions
    |   +-- formulary.R  # Reusable analysis recipes
    |   +-- data_io.R    # Data import/export
    |   +-- table_helpers.R  # Table builders
    |   +-- plot_helpers.R   # Plot constructors
    +-- data/raw/        # Raw data (never modify)
    +-- data/processed/  # Cleaned data
    +-- docs/            # Rendered HTML site
    +-- output/          # Figures, tables, models
    +-- report/          # Quarto report template

### Working with Your Project

After creation, use standard workflowr commands:

``` r

# Render all Rmd files
workflowr::wflow_build()

# Commit and publish
workflowr::wflow_publish("analysis/*.Rmd", message = "Initial build")

# Check status
workflowr::wflow_status()

# View the site
workflowr::wflow_view()

# Set up GitHub Pages
workflowr::wflow_use_github("username", "repo")
```

### Customization

- **CSS**: Edit `assets/theme.css` to customize styling
- **New Rmd files**: Add to `analysis/` and update `_site.yml` navbar
- **Navbar**: Edit `analysis/_site.yml`
- **ggplot theme**: Modify `theme_wf()` in `code/00_setup.R`

## Use of LLM tools

Portions of this package were prepared with assistance from large
language model tooling for narrowly defined, non-authorial tasks:
copyediting, prose smoothing, Markdown/LaTeX formatting, scaffolding of
boilerplate files (CI configs, build scripts), code refactoring. The
tools used were [Chat
AI](https://kisski.gwdg.de/leistungen/2-02-llm-service/), the LLM
service of KISSKI (GWDG), and a self-hosted **Mistral Small (24B,
Apache-2.0)** run locally via [Ollama](https://ollama.com/) and the
`ollamar` R package — local inference only, with no data sent to third
parties for the self-hosted model.

All scientific claims, methodological choices, analyses,
interpretations, and conclusions are the author’s own. No LLM-generated
text was incorporated without review and revision, and every reference
was verified against its DOI, arXiv ID, or ISBN.
