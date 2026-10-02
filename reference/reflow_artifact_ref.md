# Reference a declared artifact in a dependency plan

References identify logical inventory names, not arbitrary output paths.

## Usage

``` r
reflow_artifact_ref(stage, artifact, read = FALSE)
```

## Arguments

- stage:

  Producer stage identifier.

- artifact:

  One logical artifact name in the producer inventory.

- read:

  Read the verified artifact as RDS; otherwise pass its path.

## Value

A deferred artifact reference.
