# Get Individuals and Households Program (IHP) registrations

Get Individuals and Households Program (IHP) registrations

## Usage

``` r
get_ihp_registrations(
  state_fips = NULL,
  file_name = "IndividualsAndHouseholdsProgramValidRegistrationsV2_2025_09_26.parquet",
  api = FALSE,
  outpath = NULL
)
```

## Arguments

- state_fips:

  A character vector of two-letter state abbreviations. If NULL
  (default), return data for all 51 states. Otherwise return data for
  the specified states.

- file_name:

  The name (not the full path) of the Box file containing the raw data.

- api:

  If TRUE, query the API. If FALSE (default), read from disk.

- outpath:

  The path to save the parquet-formatted datafile. Applicable only when
  `api = FALSE`.

## Value

A dataframe comprising IHP registrations

## Examples

``` r
if (FALSE) { # \dontrun{
get_ihp_registrations(
   state_fips = "NJ",
   api = TRUE)
} # }
```
