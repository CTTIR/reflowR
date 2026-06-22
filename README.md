# reflowR <img src="man/figures/logo.png" align="right" height="139" alt="reflowR logo" />

> Themed extensions for workflowr research workflows

<!-- badges: start -->
[![R-CMD-check](https://github.com/CTTIR/reflowR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CTTIR/reflowR/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/CTTIR/reflowR/actions/workflows/pkgdown.yaml/badge.svg)](https://cttir.github.io/reflowR/)
[![CRAN status](https://www.r-pkg.org/badges/version/reflowR)](https://CRAN.R-project.org/package=reflowR)
[![Codecov test coverage](https://codecov.io/gh/CTTIR/reflowR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/CTTIR/reflowR?branch=main)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/reflowR)](https://cran.r-project.org/package=reflowR)
[![CRAN downloads total](https://cranlogs.r-pkg.org/badges/grand-total/reflowR)](https://cran.r-project.org/package=reflowR)
[![License: MIT](https://img.shields.io/badge/license-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview

**reflowR** extends the [workflowr](https://workflowr.github.io/workflowr/) package with themed color schemes, structured analysis pipeline templates, and utility scripts for reproducible research. It wraps `workflowr::wflow_start()` to create a standard workflowr project, then layers on a complete theming and scaffolding system.

### Theming

- **5 color schemes** -- clinical (red), basic research (steelblue), code/statistics (forest green), special (purple), other (grey) -- each with a custom SVG icon, 8-color palette, and full CSS/SCSS theme
- **ggplot2 theme + table styling** that automatically matches the selected scheme
- **Themed navbar** with gradient, icon, and pipeline dropdown menu

### Scaffolding

- **3 depth presets** -- minimal (3 steps), standard (8 steps), extended (12 steps) -- providing a structured Rmd analysis pipeline
- **6 utility scripts** in `code/` -- setup, utils, formulary, data I/O, table helpers, plot helpers
- **Quarto report template** with dual HTML/PDF output

All generated projects remain fully compatible with standard workflowr commands (`wflow_build()`, `wflow_publish()`, etc.).

## Installation

```r
# install.packages("pak")
pak::pak("cttir/reflowR")
# workflowr is installed automatically as a dependency
```

Or using remotes:

```r
remotes::install_github("cttir/reflowR")
```

## Quick Start

```r
library(reflowR)

# Create a clinical research project with the standard 8-step pipeline
reflow_init(
  directory = "~/projects/my_analysis",
  author    = "Your Name",
  email     = "you@example.com",
  scheme    = "clinical",
  depth     = "standard"
)

# Then use standard workflowr commands:
workflowr::wflow_build()
workflowr::wflow_publish("analysis/*.Rmd", message = "Initial build")
```

## Color Schemes

| Scheme | Label | Primary | Use Case |
|--------|-------|---------|----------|
| `clinical` | Clinical Research | `#C8102E` | Clinical trials, patient data |
| `basic` | Basic Research | `#4682B4` | Lab research, general science |
| `code` | Code & Statistics | `#228B22` | Software dev, statistical methods |
| `special` | Special | `#6A0DAD` | Special projects, reviews |
| `other` | Other | `#2C2C2C` | Neutral, multipurpose |

Preview any scheme interactively:

```r
reflow_preview("clinical")
reflow_schemes()
```

## Depth Presets

| Preset | Steps | Best For |
|--------|-------|----------|
| `minimal` | 3 | Quick explorations, side analyses |
| `standard` | 8 | Standard research projects, publications |
| `extended` | 12 | Clinical trials, comprehensive studies |

```r
reflow_presets()
```

## How It Works

reflowR calls `workflowr::wflow_start()` to create the standard workflowr scaffold, then enhances it with themed files. The result is a standard workflowr project -- all workflowr commands work as expected.

## Getting Help

- Browse the [function reference](https://cttir.github.io/reflowR/reference/index.html)
- Read the [Getting Started vignette](https://cttir.github.io/reflowR/articles/getting-started.html)
- File issues at [GitHub Issues](https://github.com/cttir/reflowR/issues)

## Citation

```r
citation("reflowR")
```

## Contributing

Contributions are welcome. Please open an issue first to discuss what you would like to change, then submit a pull request.

## Use of LLM tools

Portions of this package were prepared with assistance from large language model tooling for
narrowly defined, non-authorial tasks: copyediting, prose smoothing, Markdown/LaTeX formatting,
scaffolding of boilerplate files (CI configs, build scripts), code refactoring. The tools used were [Chat AI](https://kisski.gwdg.de/leistungen/2-02-llm-service/),
the LLM service of KISSKI (GWDG), and a self-hosted **Mistral Small (24B, Apache-2.0)** run locally via
[Ollama](https://ollama.com/) and the `ollamar` R package — local inference only, with no data sent to
third parties for the self-hosted model.

All scientific claims, methodological choices, analyses, interpretations, and conclusions are the
author's own. No LLM-generated text was incorporated without review and revision, and every reference
was verified against its DOI, arXiv ID, or ISBN.

## License

MIT
