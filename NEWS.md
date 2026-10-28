# reflowR (development version)

* `_workflowr.yml` records the installed workflowr version instead of a fixed
  1.7.2.

* `reflow_init()` checks all files it adds before writing anything and stops
  with a clear message if one already exists. A repeated call with
  `existing = TRUE` used to overwrite `_site.yml` and the theme and then fail
  on the icon, leaving the project partly updated.

* `reflow_init()` escapes the project name and author in the YAML headers of the
  generated pages and report, so quotes, colons or backslashes in them no
  longer produce invalid front matter.

* The Zenodo DOI is no longer listed in the README and `CITATION.cff`; the
  record is not published yet, and the DOI will be added once it is.

* Package closures (artifact definitions, imaging plans and bounded runs)
  follow Depends and Imports only. LinkingTo packages supply headers at build
  time and need not be installed; their effect is in the hashed compiled code.

* Site schemes are now the nine CTTIR v5 topic families (spectral, spatial,
  omics, clinical, methods, simulation, evidence, platform and brand), built
  from `inst/cttir/design_tokens.csv`, the token table shared with hexmakR and
  themakR. `reflow_init()` defaults to `brand`. The earlier names remain
  accepted with a one-time message: `basic` is `brand`, `code` is `methods`,
  `special` is `evidence` and `other` is `platform`; `clinical` keeps its name
  with the v5 vermillion. Existing scheme fields remain and gain explicit
  light/dark surface, tint, code token and font fields.
* Generated CSS and SCSS follow the themakR surface: flat navbar on the page
  background, light and dark modes, Inter and JetBrains Mono font stacks
  without downloads, links in the link token and the family accent only for
  identity rules. The gradient navbar and emoji titles are gone; each family
  has an R-drawn hex icon (`tools/branding/family-icons.R`).
* Generated setup, utility and plot scripts, the preview, the startup message
  and new strict profiles take colours and fonts from the v5 scheme. Strict
  declarations recorded with the earlier palette keep their colour and
  stylesheet and still validate.
* The generated `_site.yml` is valid YAML again and renders the site into
  `docs/`, as workflowr expects.
* `reflow_init()` writes the site stylesheet to `analysis/theme.css` so that
  workflowr copies it into `docs/`; the earlier `../assets/theme.css` link
  pointed outside the published site.

* Standardize the named v5 package logo across README, documentation pages
  and available application headers, with matching light and dark artwork.

* Artifact definitions bind each package of the closure by one digest of its
  folder layout and file hashes instead of storing every file hash, so graph
  journals no longer grow by megabytes per node. The definition schema is now
  `reflow_artifact_definition_2`; generations written before cannot be reused
  as `previous`.

* `options(reflowR.input_hash_cache = TRUE)` reuses the hashes of tracked
  input files within one R process in the same way as the package hash cache;
  tree walks check links of a folder's entries in one call.
* Artifact fingerprints hash files in one call (`tools::sha256sum()` on R >= 4.5.0,
  same values) and walk package trees with one type query per folder.
  `options(reflowR.package_hash_cache = TRUE)` reuses installed-package file
  hashes within one R process while device, inode, size, modification and
  change times are unchanged; it is off by default and inputs are always
  re-read.

* Add `reflow_call_writer()`, a generic artifact writer that calls one exported
  function of an installed package and saves its value as RDS with an optional
  tab-separated copy, and `reflow_call_stage()`, which declares such a stage and
  fingerprints the called package. Existing artifact APIs are unchanged.

* Add a stage-call restart for external condition handlers to reject a condition
  through the imaging executor's existing durable failure receipts. Default
  warning handling and the run/resume interfaces remain unchanged.

* Add an opt-in `strict-cttir-v1` composition with three fixed data controls,
  immutable stage entrypoints, complete installed cttiR file pins and current
  execution receipts. The original `strict-v1` defaults remain unchanged.

* Add explicit categorical numeric tiles with fixed dimensions, equal aspect
  and an optional legend text size independent of the base theme.

* Numeric tiles can explicitly preserve x-axis blank-element inheritance with
  `tile$axis_text_x_inherit_blank`; omitted and NULL values retain existing
  recipe bytes and plotting defaults. Other figure types are unchanged.

- Add opt-in numeric point layouts with wrapped free-y panels, explicit point opacity, and a separate keyed corner annotation table. Existing recipes and strict finite input validation remain unchanged.

* Allow explicitly declared finite bounded-stage deadlines from 1 through 86400 seconds. Guardian liveness, transport and cleanup intervals remain unchanged; this ceiling is not a long-duration reliability guarantee.

- Preserve the authenticated legacy figure fixture bytes across Git checkout line-ending settings.

- Add supplied numeric-point recipes with explicit fixed axes, categorical colours and facets, optional horizontal references and annotation policies. Existing recipe defaults remain unchanged; device-visible annotation coverage requires separate rendering checks.

* Add optional serial bounded graph run/resume entry points with per-stage resource policies and separately bound guardian provenance. Graph and control locks retain explicit recovery requirements.

* Finalized figure recipes accept an optional root text size without changing geometry defaults.

- Add optional literal categorical axis and facet display-label maps to finalized figure recipes without changing scientific payloads or default recipes.

- Add an explicit immutable generation registry and compact always-verified targets adapter. Interrupted selections require reconciliation; cached pointers remain unverified until consumption checks. Every completed generation remains verified even if explicitly closed failed before publication. This serial Linux-local increment does not add resource supervision or complete workflow orchestration.

- Add optional Linux guardian-backed artifact run and explicit resume APIs.
  Address-space limits are per process and thread settings are requests;
  guardian loss remains an unresolved failure requiring external cleanup.

- Added a serial artifact dependency core with deferred logical references,
  authenticated producer lineage and immutable generations that reuse unchanged
  bundles in place. Failed attempts require explicit reconciliation. Resource
  enforcement and targets integration remain outside this dependency core.

* Add a separate declared artifact bundle API for trusted installed writers,
  complete file/resource fingerprints, immutable accepted receipts and verified
  reuse. Failed attempts require explicit process reconciliation before retry;
  this is not a scheduler, sandbox or complete artifact DAG.

* Allow an explicit right-side facet-strip text angle in finalized figure
  recipes; the default preserves the existing -90-degree presentation.

* Build finalized tile heatmaps and effect-point plots from explicit keyed
  recipes, supplied categorical orders and midpoints, without aggregation or
  filtering. Keep scientific payload hashes separate from styling/provenance.

* Record common numerical thread environment settings in imaging definitions
  and runtime fingerprints; reject resume when those settings change.

* Add a sequential imaging plan/run/resume API for explicit exported package calls,
  upstream object references and tracked file or directory inputs.
* Record content, configuration, installed code, runtime and result hashes in
  durable receipts; reject stale or corrupt caches and concurrent run attempts.
  Retries preserve prior attempt artifacts. These APIs do not initialize Git,
  publish, download data or install packages.
* This first orchestration slice returns R objects; resource scheduling, external
  engine qualification, dashboards and report recipes remain separate work.

* Added a versioned strict research workflow with separate domain, data-origin
  and design declarations, canonical stage ordering and established colours.
* Added inert scaffold creation, external hash review, explicit execution
  readiness and stage provenance validation. These checks do not establish
  scientific or legal approval.

# reflowR 0.1.0

Initial release.

* `reflow_init()` -- main function wrapping `workflowr::wflow_start()` with theming.
* Five color schemes: clinical (red), basic (steelblue), code (forest green), special (purple), other (grey).
* Three depth presets: minimal (3 steps), standard (8 steps), extended (12 steps).
* Custom SVG icons per scheme embedded in navbar.
* CSS/SCSS theme generation from color scheme definitions.
* `reflow_preview()` for interactive color preview.
* `reflow_schemes()` and `reflow_presets()` convenience functions.
* Full compatibility with workflowr commands (wflow_build, wflow_publish, etc.).
* Two vignettes: Getting Started and Color Schemes Reference.
