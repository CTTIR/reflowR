# Verify a cached generation reference

Refuses incomplete or stale selections before using the existing graph
resume verification. Verification acquires and releases locks but does
not dispatch writers or mutate completed payloads or journals.

## Usage

``` r
reflow_artifact_selection_verify(pointer, plan)
```

## Arguments

- pointer:

  A compact artifact selection reference.

- plan:

  The current artifact plan.

## Value

The verified graph descriptor for immediate consumption.

## Examples

``` r
# The selector is qualified for local Linux file systems only
if (identical(Sys.info()[["sysname"]], "Linux")) {
  plan <- reflow_artifact_plan(reflow_artifact_stage(
    "lines", "base", "writeLines",
    args = list(text = c("alpha", "beta")),
    outputs = list(con = list(path = "lines.txt", type = "file")),
    inventory = c(lines = "lines.txt")
  ))
  root <- tempfile("selection-", tmpdir = normalizePath(tempdir(), mustWork = TRUE))
  dir.create(root)
  pointer <- reflow_artifact_select(plan, file.path(root, "registry"),
                                    initialize = TRUE)
  # Verify immediately before reading the selected files
  graph <- reflow_artifact_selection_verify(pointer, plan)
  readLines(file.path(graph$nodes$lines$descriptor$bundle, "lines.txt"))
  unlink(root, recursive = TRUE)
}
```
