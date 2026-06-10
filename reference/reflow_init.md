# Initialize a Themed Workflowr Project

Wraps
[`wflow_start`](https://workflowr.github.io/workflowr/reference/wflow_start.html)
to create a standard workflowr project, then extends it with a themed
color scheme, structured Rmd analysis pipeline, utility scripts, and a
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
  scheme = c("basic", "clinical", "code", "special", "other"),
  depth = c("standard", "minimal", "extended"),
  seed = as.integer(format(Sys.Date(), "%Y%m%d")),
  github_url = NULL,
  existing = FALSE,
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

  Character. Color scheme to apply. One of `"clinical"`, `"basic"`,
  `"code"`, `"special"`, `"other"`. Defaults to `"basic"`.

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

  Logical. Is the `directory` an existing project? Passed to
  `workflowr::wflow_start(existing = ...)`. Default `FALSE`.

- git:

  Logical. Initialize git repository and make first commit? Default
  `TRUE`.

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

**Phase 2 – reflowR theming:** Replaces `_site.yml`, `index.Rmd`, and
`about.Rmd` with themed versions; adds the analysis pipeline Rmd files,
CSS/SCSS, icon, setup script, utility script, Quarto report template,
and additional subdirectories. If `git = TRUE`, initializes git and
commits all files.

## See also

[`wflow_start`](https://workflowr.github.io/workflowr/reference/wflow_start.html)
for the underlying project creation,
[`wflow_build`](https://workflowr.github.io/workflowr/reference/wflow_build.html)
to render Rmd files,
[`wflow_publish`](https://workflowr.github.io/workflowr/reference/wflow_publish.html)
to commit + build + commit,
[`reflow_schemes`](https://cttir.github.io/reflowR/reference/reflow_schemes.md)
and
[`reflow_presets`](https://cttir.github.io/reflowR/reference/reflow_presets.md)
for available options.

## Examples

``` r
if (FALSE) { # \dontrun{
library(reflowR)

# Clinical trial project with full pipeline
reflow_init(
  directory = "~/projects/sepsis_trial",
  author    = "Raban Heller",
  email     = "raban.heller@outlook.com",
  scheme    = "clinical",
  depth     = "standard"
)

# Quick stats project
reflow_init(
  directory = "~/projects/simulation_study",
  author    = "Raban Heller",
  scheme    = "code",
  depth     = "minimal"
)

# Then use standard workflowr commands:
workflowr::wflow_build()
workflowr::wflow_publish("analysis/*.Rmd", message = "Initial build")
} # }
```
