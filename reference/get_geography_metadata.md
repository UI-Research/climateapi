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

A tibble containing geographic metadata. The structure varies by
`geography_type`:

- For "county":

  Returns county-level data with columns: `state_code` (2-digit FIPS),
  `state_name`, `state_abbreviation` (2-letter USPS),
  `state_population`, `county_code` (5-digit FIPS), `county_name`,
  `county_population`.

- For "state":

  Returns state-level data with columns: `state_abbreviation`,
  `state_code`, `state_name` (one row per state, no county information).

Population data are sourced from the ACS 5-year estimates for the
specified `year`.
