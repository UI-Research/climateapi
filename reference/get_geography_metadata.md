# Get geography metadata about states or counties

Get geography metadata about states or counties

## Usage

``` r
get_geography_metadata(geography_type = c("state", "county"), year = 2023)
```

## Arguments

- geography_type:

  One of c("state", "county").

- year:

  The year for which to obtain state/county metadata. Cannot be greater
  than the most recent year supported by
  [`library(tidycensus)`](https://walker-data.com/tidycensus/) for the
  5-year ACS.

## Value

A data frame containing metadata about the specified geography type and
area.
