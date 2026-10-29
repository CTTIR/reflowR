# Execute or verify an immutable artifact graph generation

A new generation can reference unchanged nodes in a completed prior
generation without copying their files. A node is reused only when its
complete resolved definition and producer lineage match. Changed inputs
never overwrite a prior generation. Resume requires the same plan,
inputs and runtime and rechecks accepted artifacts before dispatching
any writer.

## Usage

``` r
reflow_artifact_plan_run(plan, directory, previous = NULL)

reflow_artifact_plan_resume(plan, directory, reconciliations = list())
```

## Arguments

- plan:

  A
  [`reflow_artifact_plan()`](https://cttir.github.io/reflowR/reference/reflow_artifact_plan.md).

- directory:

  Fresh generation directory, or existing one for resume.

- previous:

  Optional completed generation to verify for in-place reuse.

- reconciliations:

  Named list keyed by failed stage identifiers. Each element has exactly
  `reconciled_attempt` and `reconciliation`, as in
  [`reflow_artifact_resume()`](https://cttir.github.io/reflowR/reference/reflow_artifact_run.md).
  No process absence is inferred automatically.

## Value

A graph descriptor containing authenticated node descriptors.

## Details

Execution is serial and trusted, not sandboxed or resource supervised.
Stale locks require external inspection; they are never stolen. Failed
child attempts retain the explicit reconciliation contract of
[`reflow_artifact_resume()`](https://cttir.github.io/reflowR/reference/reflow_artifact_run.md).
Atomic writes provide rename visibility, not power-loss durability. Keep
prior generations available while referenced.

## Examples

``` r
plan <- reflow_artifact_plan(
  reflow_artifact_stage(
    "data", "base", "saveRDS",
    args = list(object = c("alpha", "beta")),
    outputs = list(file = list(path = "labels.rds", type = "file")),
    inventory = c(labels = "labels.rds")
  ),
  reflow_artifact_stage(
    "report", "base", "writeLines",
    args = list(text = reflow_artifact_ref("data", "labels", read = TRUE)),
    outputs = list(con = list(path = "report.txt", type = "file")),
    inventory = c(report = "report.txt")
  )
)
root <- tempfile("graph-", tmpdir = normalizePath(tempdir(), winslash = "/", mustWork = TRUE))
dir.create(root)
graph <- reflow_artifact_plan_run(plan, file.path(root, "first"))
readLines(file.path(graph$nodes$report$descriptor$bundle, "report.txt"))
#> [1] "alpha" "beta" 

# Resume checks the accepted files again without calling any writer
identical(reflow_artifact_plan_resume(plan, file.path(root, "first")), graph)
#> [1] TRUE
unlink(root, recursive = TRUE)
```
