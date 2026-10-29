# Initialize a Themed workflowr Project

Wraps
[`wflow_start`](https://workflowr.github.io/workflowr/reference/wflow_start.html)
to create a standard workflowr project, then extends it with a themed
colour scheme, structured Rmd analysis pipeline, utility scripts, and a
Quarto report template.

## Usage

``` r
reflow_init(
  directory,
  name = NULL,
  author = "Author Name",
  email = NULL,
  affiliation = NULL,
  orcid = NULL,
  website = NULL,
  scheme = c("brand", "spectral", "spatial", "omics", "clinical", "methods",
    "simulation", "evidence", "platform", "basic", "code", "special", "other"),
  depth = c("standard", "minimal", "extended"),
  seed = as.integer(format(Sys.Date(), "%Y%m%d")),
  github_url = NULL,
  existing = FALSE,
  overwrite = FALSE,
  git = TRUE,
  change_wd = rlang::is_interactive(),
  user.name = NULL,
  user.email = NULL,
  open = rlang::is_interactive()
)
```

## Arguments

- directory:

  Character. Path where the project will be created. Passed directly to
  [`workflowr::wflow_start()`](https://workflowr.github.io/workflowr/reference/wflow_start.html).

- name:

  Character. Project name (used in titles and navbar). If `NULL`,
  derived from the directory basename.

- author:

  Character. Full name of the primary author.

- email:

  Character. Author's email address (optional).

- affiliation:

  Character. Author's institutional affiliation (optional).

- orcid:

  Character. Author's ORCID iD (optional, format:
  "0000-0000-0000-0000").

- website:

  Character. URL to author's or project website (optional).

- scheme:

  Character. CTTIR v5 family whose colours, icon and fonts style the
  site: one of `"brand"` (default), `"spectral"`, `"spatial"`,
  `"omics"`, `"clinical"`, `"methods"`, `"simulation"`, `"evidence"` or
  `"platform"`. The earlier names `"basic"`, `"code"`, `"special"` and
  `"other"` remain accepted as aliases of brand, methods, evidence and
  platform. See
  [`reflow_schemes`](https://cttir.github.io/reflowR/dev/reference/reflow_schemes.md).

- depth:

  Character. Rmd pipeline depth. One of `"minimal"`, `"standard"`,
  `"extended"`. Defaults to `"standard"`.

- seed:

  Integer. Random seed for workflowr reproducibility. Defaults to
  `as.integer(format(Sys.Date(), "%Y%m%d"))`.

- github_url:

  Character. GitHub repository URL (optional). Adds a GitHub link to the
  navbar.

- existing:

  Logical. Is the `directory` an existing project or folder? Passed to
  `workflowr::wflow_start(existing = ...)`, which adds the missing
  workflowr files and leaves present ones alone. Default `FALSE`. Files
  that reflowR writes are replaced only with `overwrite = TRUE`. Files
  already in the folder are not committed (see `git`).

- overwrite:

  Logical. Replace files that reflowR writes if they already exist in
  `directory`: `README.md`, `_workflowr.yml`, `analysis/_site.yml`,
  `analysis/index.Rmd`, `analysis/about.Rmd` and the theme, icon,
  script, pipeline and report files. Default `FALSE`: the function then
  stops before writing anything and lists the files. Other workflowr
  files are never replaced. Re-theming at a different `depth` leaves the
  pipeline files of the earlier depth that the new one does not write in
  `analysis/`; remove them yourself if they are no longer needed.

- git:

  Logical. Initialize a git repository (or use the one in an existing
  folder) and commit the files that this call writes? Other files in
  `directory`, and changes to them, are not committed. If the repository
  already has staged changes, no commit is made and a warning says so.
  Default `TRUE`.

- change_wd:

  Logical. Change working directory to the new project? Default `TRUE`
  in interactive sessions.

- user.name:

  Character. Git user name (optional). Passed to
  [`workflowr::wflow_start()`](https://workflowr.github.io/workflowr/reference/wflow_start.html).

- user.email:

  Character. Git user email (optional). Passed to
  [`workflowr::wflow_start()`](https://workflowr.github.io/workflowr/reference/wflow_start.html).

- open:

  Logical. Open the project in RStudio after creation? Default `TRUE` in
  interactive sessions.

## Value

Invisibly returns the path to the created project directory.

## Details

The function proceeds in two phases:

**Phase 1 – workflowr base:** Calls
[`workflowr::wflow_start()`](https://workflowr.github.io/workflowr/reference/wflow_start.html)
with `git = FALSE` to create the standard workflowr scaffold without
committing yet.

**Phase 2 – reflowR theming:** Replaces `_site.yml`, `index.Rmd`,
`about.Rmd`, `_workflowr.yml` and `README.md` with themed versions; adds
the analysis pipeline Rmd files, CSS/SCSS, icon, setup script, utility
script, Quarto report template, and additional subdirectories. If
`git = TRUE`, initializes git and commits the files written by both
phases.

If any of the files that Phase 2 writes already exists in `directory`
before the call, for example the README or `_site.yml` of an existing
workflowr project or the files of an earlier call, the function stops
before writing anything and lists them, so a project is never changed
silently or left partly updated. Set `overwrite = TRUE` to replace them
with the reflowR versions. It also stops before writing anything if one
of these files, or the folder it goes in, is read-only.

## See also

[`wflow_start`](https://workflowr.github.io/workflowr/reference/wflow_start.html)
for the underlying project creation,
[`wflow_build`](https://workflowr.github.io/workflowr/reference/wflow_build.html)
to render Rmd files,
[`wflow_publish`](https://workflowr.github.io/workflowr/reference/wflow_publish.html)
to commit + build + commit,
[`reflow_schemes`](https://cttir.github.io/reflowR/dev/reference/reflow_schemes.md)
and
[`reflow_presets`](https://cttir.github.io/reflowR/dev/reference/reflow_presets.md)
for available options.

## Examples

``` r
# A clinical project with the three-step pipeline, in a temporary folder
project <- file.path(tempdir(), "my_project")
reflow_init(
  directory = project,
  author    = "Your Name",
  email     = "you@example.com",
  scheme    = "clinical",
  depth     = "minimal",
  git       = FALSE,
  change_wd = FALSE,
  open      = FALSE
)
#> 
#> ── Creating reflowR project: "my_project" ──────────────────────────────────────
#> ℹ Scheme: "clinical" | Depth: "minimal"
#> 
#> ── Phase 1: workflowr scaffold ──
#> 
#> ── Phase 2: reflowR theming ──
#> 
#> → Creating directory structure...
#> → Generating themed _site.yml...
#> → Generating theme CSS/SCSS...
#> → Adding scheme icon...
#> → Configuring workflowr seed...
#> → Generating R scripts...
#> → Generating code library...
#> → Generating 3 analysis Rmd files...
#> → Generating index and about pages...
#> → Generating Quarto report template...
#> → Generating README...
#> 
#> ── Done! ──
#> 
#> ✔ Project created at /tmp/Rtmp1OxfDf/my_project
#> ℹ Scheme: "Clinical science" | Pipeline: "Minimal (3 steps)"
#> 
#> ℹ Next steps:
#> 1. Open my_project.Rproj in RStudio
#> 2. Place your data in data/raw/
#> 3. Run `workflowr::wflow_build()` to render
#> 4. Run `workflowr::wflow_publish()` to commit + build
list.files(file.path(project, "analysis"))
#> [1] "01_read_data.Rmd" "02_eda.Rmd"       "03_report.Rmd"    "_site.yml"       
#> [5] "about.Rmd"        "index.Rmd"        "license.Rmd"      "theme.css"       

# In a real project, continue with the standard workflowr commands, e.g.
# workflowr::wflow_build() and workflowr::wflow_publish().
unlink(project, recursive = TRUE)
```
