# Call one installed export and save its value

A generic artifact writer: calls exactly one exported function of an
installed package with the given arguments and saves the returned object
as RDS. Use it as the writer of a
[`reflow_artifact_stage()`](https://cttir.github.io/reflowR/dev/reference/reflow_artifact_stage.md)
(see
[`reflow_call_stage()`](https://cttir.github.io/reflowR/dev/reference/reflow_call_stage.md))
so that configured calculations need no writer code of their own.
Artifact references in `args` are resolved by the executor before this
function runs; with `read = TRUE` they arrive as objects.

## Usage

``` r
reflow_call_writer(fun, args = list(), path, tsv = NULL)
```

## Arguments

- fun:

  One string `"package::function"` naming an exported function.

- args:

  Uniquely named list of arguments for `fun`.

- path:

  Output path of the RDS file.

- tsv:

  Optional output path of the tab-separated copy, or NULL.

## Value

The output paths, invisibly.

## Details

The function is trusted and is not sandboxed. It must not write files of
its own. Declare its package in the stage `packages` so that its
installed files are fingerprinted;
[`reflow_call_stage()`](https://cttir.github.io/reflowR/dev/reference/reflow_call_stage.md)
does this.

A tab-separated copy is written when `tsv` is a path. A data frame whose
columns are all atomic vectors is written with a header row, `NA` for
missing values and double quotes only around fields that contain a tab,
line break or double quote. Any other value gives an empty file, so the
declared inventory stays fixed; declare that artifact in `allow_empty`.
The RDS file is the exact value; the table is a readable copy.

## Examples

``` r
out <- tempfile(fileext = ".rds")
reflow_call_writer("utils::head", list(x = datasets::mtcars, n = 3), out)
readRDS(out)
#>                mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4     21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710    22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
```
