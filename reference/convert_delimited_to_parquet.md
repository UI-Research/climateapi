# Convert raw data to parquet to conserve memory / speed subsequent operations

Convert raw data to parquet to conserve memory / speed subsequent
operations

## Usage

``` r
convert_delimited_to_parquet(
  inpath,
  outpath = NULL,
  delimit_character = ",",
  subsetted_columns = NULL,
  dataset = NULL
)
```

## Arguments

- inpath:

  The local path to read CSV data from.

- outpath:

  The local path to write parquet data to.

- delimit_character:

  The delimiting character of the raw data.

- subsetted_columns:

  The columns to include in the outputted parquet data.

- dataset:

  NULL by default. Alternately, one of c("nfip_policies",
  "ihp_registrations"). If not null, this will be used to select the
  columns that are returned.

## Value

Nothing. Parquet data are written to local path.
