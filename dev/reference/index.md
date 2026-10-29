# Package index

## Finalized-value figures

Render supplied values with explicit levels and no numerical reductions.

- [`reflow_figure_recipe()`](https://cttir.github.io/reflowR/dev/reference/reflow_figure_recipe.md)
  : Declare a Finalized-Value Figure Recipe
- [`reflow_figure_plot()`](https://cttir.github.io/reflowR/dev/reference/reflow_figure_plot.md)
  : Build a Plot from a Finalized-Value Figure Recipe

## Recorded imaging workflows

Declare package calls and execute or resume verified object workflows.

- [`reflow_imaging_stage()`](https://cttir.github.io/reflowR/dev/reference/reflow_imaging_stage.md)
  : Declare an imaging workflow stage
- [`reflow_imaging_ref()`](https://cttir.github.io/reflowR/dev/reference/reflow_imaging_ref.md)
  [`reflow_imaging_input()`](https://cttir.github.io/reflowR/dev/reference/reflow_imaging_ref.md)
  : Reference an upstream result or an input file
- [`reflow_imaging_plan()`](https://cttir.github.io/reflowR/dev/reference/reflow_imaging_plan.md)
  : Assemble a sequential imaging dependency graph
- [`reflow_imaging_run()`](https://cttir.github.io/reflowR/dev/reference/reflow_imaging_run.md)
  [`reflow_imaging_resume()`](https://cttir.github.io/reflowR/dev/reference/reflow_imaging_run.md)
  : Execute or resume a recorded imaging plan
- [`reflow_artifact_spec()`](https://cttir.github.io/reflowR/dev/reference/reflow_artifact_spec.md)
  : Declare one immutable file-producing bundle
- [`reflow_artifact_run()`](https://cttir.github.io/reflowR/dev/reference/reflow_artifact_run.md)
  [`reflow_artifact_resume()`](https://cttir.github.io/reflowR/dev/reference/reflow_artifact_run.md)
  : Execute or verify one declared artifact bundle
- [`reflow_artifact_run_bounded()`](https://cttir.github.io/reflowR/dev/reference/reflow_artifact_run_bounded.md)
  [`reflow_artifact_resume_bounded()`](https://cttir.github.io/reflowR/dev/reference/reflow_artifact_run_bounded.md)
  : Run an artifact with an optional Linux guardian
- [`reflow_artifact_ref()`](https://cttir.github.io/reflowR/dev/reference/reflow_artifact_ref.md)
  : Reference a declared artifact in a dependency plan
- [`reflow_artifact_stage()`](https://cttir.github.io/reflowR/dev/reference/reflow_artifact_stage.md)
  : Declare a deferred artifact writer
- [`reflow_artifact_plan()`](https://cttir.github.io/reflowR/dev/reference/reflow_artifact_plan.md)
  : Declare a serial artifact dependency plan
- [`reflow_call_writer()`](https://cttir.github.io/reflowR/dev/reference/reflow_call_writer.md)
  : Call one installed export and save its value
- [`reflow_call_stage()`](https://cttir.github.io/reflowR/dev/reference/reflow_call_stage.md)
  : Declare a stage that calls one installed export
- [`reflow_artifact_plan_run()`](https://cttir.github.io/reflowR/dev/reference/reflow_artifact_plan_run.md)
  [`reflow_artifact_plan_resume()`](https://cttir.github.io/reflowR/dev/reference/reflow_artifact_plan_run.md)
  : Execute or verify an immutable artifact graph generation
- [`reflow_artifact_plan_run_bounded()`](https://cttir.github.io/reflowR/dev/reference/reflow_artifact_plan_run_bounded.md)
  [`reflow_artifact_plan_resume_bounded()`](https://cttir.github.io/reflowR/dev/reference/reflow_artifact_plan_run_bounded.md)
  : Run or resume a serial graph with per-node Linux guardians
- [`reflow_artifact_select()`](https://cttir.github.io/reflowR/dev/reference/reflow_artifact_select.md)
  : Select an immutable artifact graph generation
- [`reflow_artifact_selection_verify()`](https://cttir.github.io/reflowR/dev/reference/reflow_artifact_selection_verify.md)
  : Verify a cached generation reference
- [`reflow_artifact_target()`](https://cttir.github.io/reflowR/dev/reference/reflow_artifact_target.md)
  : Always verify a compact artifact selection in targets

## Project creation

Create and initialize themed workflowr projects.

- [`reflow_init()`](https://cttir.github.io/reflowR/dev/reference/reflow_init.md)
  : Initialize a Themed workflowr Project

## Strict research workflows

Create and validate projects with fixed stages and externally reviewed
declarations.

- [`reflow_profile()`](https://cttir.github.io/reflowR/dev/reference/reflow_profile.md)
  : Declare a Strict Research Workflow Profile
- [`reflow_scaffold()`](https://cttir.github.io/reflowR/dev/reference/reflow_scaffold.md)
  : Plan a Strict Workflow Scaffold
- [`reflow_init_strict()`](https://cttir.github.io/reflowR/dev/reference/reflow_init_strict.md)
  : Write a Strict Workflow Project
- [`reflow_validate_strict()`](https://cttir.github.io/reflowR/dev/reference/reflow_validate_strict.md)
  : Validate a Strict Project Against an External Declaration

## Schemes and presets

Explore available color schemes and pipeline depth presets.

- [`reflow_scheme()`](https://cttir.github.io/reflowR/dev/reference/reflow_scheme.md)
  : Get a Colour Scheme Definition
- [`reflow_schemes()`](https://cttir.github.io/reflowR/dev/reference/reflow_schemes.md)
  : List Available Colour Schemes
- [`reflow_preset()`](https://cttir.github.io/reflowR/dev/reference/reflow_preset.md)
  : Get a Depth Preset Definition
- [`reflow_presets()`](https://cttir.github.io/reflowR/dev/reference/reflow_presets.md)
  : List Available Depth Presets
- [`reflow_preview()`](https://cttir.github.io/reflowR/dev/reference/reflow_preview.md)
  : Preview a Colour Scheme

## Utilities

Internal helpers for icon embedding.

- [`reflow_icon_base64()`](https://cttir.github.io/reflowR/dev/reference/reflow_icon_base64.md)
  : Get Base64-Encoded SVG Icon for a Scheme
