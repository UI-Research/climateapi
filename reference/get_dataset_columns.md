# Get the raw column names for a specified dataset

Get the raw column names for a specified dataset

## Usage

``` r
get_dataset_columns(dataset)
```

## Arguments

- dataset:

  The name of the dataset. One of c('nfip_policies', 'nfip_claims',
  'ihp_registrations').

## Value

A character vector containing the raw column names (in camelCase format
as they appear in the source data) to be selected when reading the
specified dataset. The columns returned are curated subsets of the full
dataset columns, excluding administrative/metadata fields. For
"nfip_policies": 11 columns matching the current per-state parquet
schema. For "nfip_claims": 19 columns needed by
[`get_nfip_claims()`](https://ui-research.github.io/climateapi/reference/get_nfip_claims.md)'s
downstream
[`transmute()`](https://dplyr.tidyverse.org/reference/transmute.html).
For "ihp_registrations": ~20 columns including disaster info, geographic
identifiers, and assistance amounts.
