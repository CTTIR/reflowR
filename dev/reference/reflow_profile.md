# Declare a Strict Research Workflow Profile

Creates a versioned declaration for a research-group project. Domain,
data origin and design are independent dimensions. A profile records
applicable stages; it does not select statistical methods or approve
data access.

## Usage

``` r
reflow_profile(
  domain,
  data_origin,
  design,
  seed = 1L,
  stages = NULL,
  profile_version = "strict-v1",
  data_access = "unknown",
  jurisdiction = "unknown",
  legal_basis = "unknown"
)
```

## Arguments

- domain:

  One of `clinical`, `basic`, `methods`, `software`, or `other`.

- data_origin:

  One of `primary` or `secondary`.

- design:

  One of `experimental`, `observational`, `synthesis`, `simulation`,
  `software`, or `other`.

- seed:

  One nonnegative integer for reproducible analysis sessions.

- stages:

  Optional named list of stage overrides. Each value has exactly
  `applicable` (logical) and `reason` (plain text) fields. An
  inapplicable stage requires a nonempty reason. Unspecified stages
  remain applicable.

- profile_version:

  `strict-v1` (default), or the explicit cttiR composition
  `strict-cttir-v1`.

- data_access:

  One of `unknown`, `public`, `controlled`, `restricted`, or
  `synthetic`. This records a declaration, not access authorization.

- jurisdiction:

  Plain-text jurisdiction or `unknown`.

- legal_basis:

  Plain-text access/use basis or `unknown`.

## Value

A declarative list of class `reflow_strict_profile`.

## Examples

``` r
p <- reflow_profile("clinical", "secondary", "observational")
p$scheme
#> [1] "clinical"
p$badge
#> [1] "SECONDARY DATA"
```
