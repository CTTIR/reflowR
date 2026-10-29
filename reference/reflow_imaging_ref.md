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

## Examples

``` r
# Pass the value of an earlier stage, or one component of it
reflow_imaging_ref("fit")
#> $stage
#> [1] "fit"
#> 
#> $select
#> character(0)
#> 
#> attr(,"class")
#> [1] "reflow_imaging_ref"
reflow_imaging_ref("fit", select = "coefficients")
#> $stage
#> [1] "fit"
#> 
#> $select
#> [1] "coefficients"
#> 
#> attr(,"class")
#> [1] "reflow_imaging_ref"

# Track an input file: its contents are hashed into the run definition
path <- tempfile(fileext = ".rds")
saveRDS(c(2, 4, 4, 5), path)
input <- reflow_imaging_input(path, read = TRUE)
input$read
#> [1] TRUE
unlink(path)
```
