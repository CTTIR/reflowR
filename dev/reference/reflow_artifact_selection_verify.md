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
