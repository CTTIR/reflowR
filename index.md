# reflowR ![reflowR logo](reference/figures/logo-with-name.png)

> Themed extensions for workflowr research workflows

## Overview

**reflowR** extends the
[workflowr](https://workflowr.github.io/workflowr/) package with themed
colour schemes, structured analysis pipeline templates, and utility
scripts for reproducible research. It wraps
[`workflowr::wflow_start()`](https://workflowr.github.io/workflowr/reference/wflow_start.html)
to create a standard workflowr project, then layers on a complete
theming and scaffolding system.

### Theming

- **9 CTTIR v5 families** – spectral, spatial, omics, clinical, methods,
  simulation, evidence, platform and brand – built from the same design
  tokens as hexmakR and themakR, each with a family hex icon, Okabe-Ito
  palette, and CSS/SCSS theme with light and dark modes
- **ggplot2 theme + table styling** that automatically matches the
  selected family
- **Flat navbar** on the page background with the family icon, an accent
  rule, and the pipeline dropdown menu

### Scaffolding

- **3 depth presets** – minimal (3 steps), standard (8 steps), extended
  (12 steps) – providing a structured Rmd analysis pipeline
- **6 utility scripts** in `code/` – setup, utils, formulary, data I/O,
  table helpers, plot helpers
- **Quarto report template** with dual HTML/PDF output

All generated projects remain fully compatible with standard workflowr
commands (`wflow_build()`, `wflow_publish()`, etc.).

## Installation

reflowR is not on CRAN yet. Install it from GitHub:

``` r

# install.packages("pak")
pak::pak("CTTIR/reflowR")
```

## Quick Start

``` r

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

## Colour Schemes

| Scheme | Label | Accent | Use Case |
|----|----|----|----|
| `spectral` | Spectral imaging | `#0072B2` | Imaging, spectroscopy |
| `spatial` | Spatial biology | `#009E73` | Tissue and spatial assays |
| `omics` | Molecular data | `#CC79A7` | Sequencing, molecular profiles |
| `clinical` | Clinical science | `#D55E00` | Clinical trials, patient data |
| `methods` | Learning & inference | `#56B4E9` | Statistical and ML methods |
| `simulation` | Systems & simulation | `#E69F00` | Simulation studies, models |
| `evidence` | Evidence & discovery | `#F0E442` | Reviews, meta-analyses |
| `platform` | Research infrastructure | `#000000` | Software, data, infrastructure |
| `brand` | Science, connected | `#0072B2` | General purpose (default) |

The earlier names `basic`, `code`, `special` and `other` remain accepted
as aliases of `brand`, `methods`, `evidence` and `platform`. Links and
text use neutral tokens in light and dark mode; the accent marks
identity only.

Preview any scheme interactively:

``` r

reflow_preview("clinical")
reflow_schemes()
```

## Depth Presets

| Preset     | Steps | Best For                                 |
|------------|-------|------------------------------------------|
| `minimal`  | 3     | Quick explorations, side analyses        |
| `standard` | 8     | Standard research projects, publications |
| `extended` | 12    | Clinical trials, comprehensive studies   |

``` r

reflow_presets()
```

## How It Works

reflowR calls
[`workflowr::wflow_start()`](https://workflowr.github.io/workflowr/reference/wflow_start.html)
to create the standard workflowr scaffold, then enhances it with themed
files. The result is a standard workflowr project – all workflowr
commands work as expected.

## Getting Help

- Browse the [function
  reference](https://cttir.github.io/reflowR/reference/index.html)
- Read the [Getting Started
  vignette](https://cttir.github.io/reflowR/articles/getting-started.html)
- File issues at [GitHub
  Issues](https://github.com/CTTIR/reflowR/issues)

## Citation

``` r

citation("reflowR")
```

## Contributing

Contributions are welcome. Please open an issue first to discuss what
you would like to change, then submit a pull request.

## License

MIT

## Recorded imaging workflows

The imaging API runs a sequential graph of exported package functions,
with explicit references to upstream R objects. It is independent of
workflowr scaffolding and does not create Git repositories or publish
anything.

``` r

plan <- reflow_imaging_plan(
  reflow_imaging_stage("summary", "base", "summary", list(object = 1:4)),
  reflow_imaging_stage("mean", "base", "mean",
                      list(x = reflow_imaging_ref("summary")))
)
run_dir <- tempfile("imaging-run-")
results <- reflow_imaging_run(plan, run_dir)
verified <- reflow_imaging_resume(plan, run_dir)
```

Use `reflow_imaging_input(path)` for tracked files or directories, or
set `read = TRUE` to pass an RDS input as an object. Use the reference’s
`select` argument to extract named nested components. Declare optional
package backends with the plan’s `packages` argument and stochastic
calls with a stage `seed`.

Receipts record configuration, input, installed code, runtime and output
hashes. Resume verifies completed results; changed inputs or corrupt
results require a new run directory. Common numerical thread environment
settings are recorded and changes invalidate resume; this does not
measure effective backend thread counts or capture in-process thread
setters. Failed or interrupted calls retry in new attempt directories. A
lock prevents concurrent execution; stale locks require manual
inspection. Inputs must remain immutable during execution. Calls must be
trusted, return plain serializable R objects, and avoid external side
effects: this is not a sandbox. Resource scheduling, file-producing
stages, external engine provenance, review dashboards and report recipes
are not yet implemented by this API.

## Use of LLM tools

Where applicable, LLM tools were used for narrowly defined,
non-authorial tasks: copyediting, prose refinement, Markdown/LaTeX
formatting, boilerplate scaffolding (such as CI configurations and build
scripts), and code refactoring. Tools included [Chat
AI](https://kisski.gwdg.de/leistungen/2-02-llm-service/), the KISSKI
(GWDG) service, and self-hosted **Mistral Small (24B, Apache-2.0)**
served through a local **vLLM architecture** on an Ubuntu Linux
workstation with Intel processors and NVIDIA GPUs. The self-hosted model
ran locally, with no data sent to third parties.

All scientific claims, methodological choices, analyses, interpretations
and conclusions remain the authors’ own. Where applicable, LLM-generated
material must be reviewed and revised before inclusion, and references
must be verified against their DOI, arXiv ID or ISBN.

Local software setup: vLLM with Python/PyTorch, plus R and RStudio.

## Branding

Editable R artwork and regeneration instructions are in
[tools/branding](https://github.com/CTTIR/reflowR/blob/main/tools/branding/README.md).
