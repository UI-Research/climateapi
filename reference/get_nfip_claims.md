# Access county-level data on NFIP claims

Retrieves National Flood Insurance Program (NFIP) claims data at the
county level, including damage amounts, payments, and building
characteristics.

## Usage

``` r
get_nfip_claims(
  county_geoids = NULL,
  file_name = "fima_nfip_claims_2025_09_09.parquet",
  api = FALSE
)
```

## Arguments

- county_geoids:

  A character vector of five-digit county codes. NULL by default; must
  be non-NULL if `api = TRUE`.

- file_name:

  The name (not the full path) of the Box file containing the raw data.

- api:

  If TRUE, query the API. FALSE by default.

## Value

A data frame comprising county-level data on current NFIP policies

- state_fips:

  A two-digit state identifier.

- state_abbreviation:

  The name of the state.

- county_geoid:

  A five-digit county identifier.

- county_name:

  The name of the county.

- year_construction:

  The original year of the construction of the building.

- year_loss:

  The year in which the flood loss occurred.

- occupancy_type:

  The occupancy type of the primary building associated with the claim.

- count_units_insured:

  The number of insured units associated with the claim.

- deductible_building:

  The total deductible for buildings, main and appurtenant.

- deductible_contents:

  The total deductible for contents.

- value_building:

  The value of the main building as estimated by an adjuster.

- value_contents:

  The value of the contents as estimated by an adjuster.

- replacement_cost_building:

  Estimated cost to replace the building as reported by the insurer.

- replacement_cost_contents:

  Estimated cost to replace the contents as reported by the insurer.

- insurance_coverage_building:

  The total insurance amount on the building.

- insurance_coverage_contents:

  The total insurance amount on the contents.

- damage_building:

  The amount of damage to a main property.

- damage_contents:

  The value of damage to contents.

- net_payment_building:

  Net building payment amount.

- net_payment_contents:

  Net contents payment amount.

- net_payment_increased_compliance:

  Net Increased Cost of Compliance (ICC) payment amount.

## Details

Data are from FEMA's OpenFEMA API. See
<https://www.fema.gov/openfema-data-page/fima-nfip-redacted-claims-v2>.
Per FEMA: This data set represents more than 2,000,000 NFIP claims
transactions. It is derived from the NFIP system of record, staged in
the NFIP reporting platform and redacted to protect policy holder
personally identifiable information. The dataset includes the 50
states + DC and the following territories: Puerto Rico, US Virgin
Islands, and Guam.

In order to filter to residential claims, filter out occupancy type:
"non-residential".

Some claims (from multi-unit buildings / condos) are associated with
multiple insured units. When calculating the number of units covered by
a claim, the analyst should use the count_units_insured column.

The example below illustrates and example of how the data set can be
summarized to show the total number of residential claims submitted in
two different counties, as well as the total damages and payments in the
same time period.

## Examples

``` r
if (FALSE) { # \dontrun{

test <- get_nfip_claims(county_geoids = c("01001", "48201")) |>
  dplyr::filter(
    year_of_loss >= 2015,  ### in the past 10 years
    !occupancy_type %in% c("non-residential")) |> ### only residential claims
  dplyr::summarize(
    .by = county_geoid,
    dplyr::across(dplyr::matches("payment"), sum, na.rm = TRUE),
    residential_claims = dplyr::n_distinct(nfip_claim_id))
} # }
```
