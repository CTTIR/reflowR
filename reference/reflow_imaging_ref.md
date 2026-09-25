# Reference an upstream result or an input file

Reference an upstream result or an input file

## Usage

``` r
reflow_imaging_ref(stage, select = character())

reflow_imaging_input(path, read = FALSE)
```

## Arguments

- stage:

  Identifier of an earlier stage.

- select:

  Character vector of nested list components to extract.

- path:

  Existing input file or directory. All contents are hashed.

- read:

  Read an RDS input as an object; otherwise pass its absolute path.

## Value

An explicit dependency reference.
