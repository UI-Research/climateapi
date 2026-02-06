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

NULL (invisibly). This function is called for its side effect of writing
a parquet file to disk at the specified `outpath` (or a path derived
from `inpath` with a .parquet extension). The function reads the input
file in chunks to handle large files efficiently, optionally subsets to
specified columns, and writes the result in Apache Parquet format using
[`arrow::write_parquet()`](https://arrow.apache.org/docs/r/reference/write_parquet.html).
