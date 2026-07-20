# Get Individuals and Households Program (IHP) registrations

Retrieves FEMA Individual and Households Program (IHP) registration
data, which captures applications for disaster assistance from
individuals and households.

## Usage

``` r
get_ihp_registrations(
  state_abbreviation = NULL,
  file_name = NULL,
  api = FALSE,
  outpath = NULL
)
```

## Arguments

- state_abbreviation:

  A character vector of two-letter state abbreviations. If NULL
  (default), return data for all 51 states. Otherwise return data for
  the specified states.

- file_name:

  The name (not the full path) of the Box file containing the raw data.
  If NULL (default), reads the most recently cached file for this
  dataset from
  [`get_openfema_cache_path()`](https://ui-research.github.io/climateapi/reference/get_openfema_cache_path.md).

- api:

  If TRUE, query the API. If FALSE (default), read from disk.

- outpath:

  The path to save the parquet-formatted datafile. Applicable only when
  `api = FALSE`.

## Value

A dataframe comprising IHP registrations. Note that records are
duplicated due to a many-to-many join with a ZCTA-to-county crosswalk;
use `allocation_factor_zcta_to_county` to properly aggregate. Columns
include:

- unique_id:

  Unique identifier for the original registration.

- allocation_factor_zcta_to_county:

  Weight for attributing registration to county.

- geoid_county:

  Five-digit county FIPS code.

- zcta_code:

  Five-digit ZIP Code Tabulation Area.

- geoid_tract:

  11-digit census tract FIPS code.

- geoid_block_group:

  12-digit census block group FIPS code.

- disaster_number:

  FEMA disaster number.

- amount_individual_housing_program:

  Total IHP assistance amount in dollars.

- amount_housing_assistance:

  Housing assistance amount in dollars.

- amount_other_needs_assistance:

  Other needs assistance amount in dollars.

- amount_rental_assistance:

  Rental assistance amount in dollars.

- amount_repairs:

  Repair assistance amount in dollars.

- amount_replacement:

  Replacement assistance amount in dollars.

- amount_personal_property:

  Personal property assistance amount in dollars.

- amount_flood_insurance_premium_paid_by_fema:

  FEMA-paid flood insurance premium.

- state_name:

  Full state name.

- state_abbreviation:

  Two-letter state abbreviation.

- state_code:

  Two-digit state FIPS code.

## Details

Data are from FEMA's OpenFEMA API. See
<https://www.fema.gov/openfema-data-page/individuals-and-households-program-valid-registrations-v2>.

## Examples

``` r
if (FALSE) { # \dontrun{
get_ihp_registrations(
   state_abbreviation = "NJ",
   api = TRUE)
} # }
```
