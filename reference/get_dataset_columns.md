# Get the raw column names for a specified dataset

Get the raw column names for a specified dataset

## Usage

``` r
get_dataset_columns(dataset)
```

## Arguments

- dataset:

  The name of the dataset. One of c('nfip_policies',
  'ihp_registrations').

## Value

A character vector containing the raw column names (in camelCase format
as they appear in the source data) to be selected when reading the
specified dataset. The columns returned are curated subsets of the full
dataset columns, excluding administrative/metadata fields. For
"nfip_policies": 20 columns including location, policy details, and
building characteristics. For "ihp_registrations": ~20 columns including
disaster info, geographic identifiers, and assistance amounts.
