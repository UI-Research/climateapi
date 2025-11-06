# Access county-level data on NFIP policies

Access county-level data on NFIP policies

## Usage

``` r
get_nfip_policies(
  state_abbreviation,
  county_geoids = NULL,
  file_name = "fima_nfip_policies_2025_10_14.parquet",
  api = FALSE
)
```

## Arguments

- state_abbreviation:

  A 2 letter state abbreviation (e.g. TX).

- county_geoids:

  A character vector of five-digit county codes.

- file_name:

  The name (not the full path) of the Box file containing the raw data.

- api:

  If TRUE, query the API. If FALSE (default), read from `file_name`.

## Value

A dataframe of project-level funding requests and awards, along with
variables that can be aggregated to the county level.

- state_fips:

  A two-digit state identifier.

- state_abbreviation:

  The two-character abbreviation of the state.

- county_code:

  The five-digit county identifier.

- county_name:

  The name of the county.

- census_tract:

  The 11-digit census tract code.

- policy_cost:

  The cost of the policy summing the calculated premium, reserve fund
  assessment, federal policy fee, and HFIAA surcharge.

- policy_count:

  The number of insured units in active status associated with the
  policy.

- policy_rated_flood_zone:

  The NFIP flood zone.

- policy_premium_total_cost:

  The policy premium; negative values indicate a refund.

- policy_date_termination:

  The date when the policy is no longer in effect, either because it was
  cancelled or lapsed.

- policy_date_effective:

  The effective date of the flood policy.

- building_occupancy_type:

  The occupancy type of the building.

- building_replacement_cost:

  The insurer's estimated cost to replace the building.

## Details

The following dataset houses information on NFIP policies (both historic
and current). In order to filter to current policies, the analyst will
need to filter on the policy_date_termination and policy_date_effective
columns.

The dataset also contains both residential and commercial policies. In
order to filter to residential policies, the analyst can filter out the
"non-residential" occupancy type.

## Examples

``` r
if (FALSE) { # \dontrun{
 test <- get_nfip_policies(
      state_abbreviation = "TX",
      county_geoids = c("48201"),
      file_name = "fima_nfip_policies_2025_10_14.parquet",
      api = FALSE) |>
    dplyr::filter(
      !occupancy_type %in% c("non-residential"), ### only residential claims,
      policy_date_termination >= as.Date("2025-10-15"),
      policy_date_effective <= as.Date("2025-10-15")) |>
    dplyr::group_by(county_geoid)|>
    dplyr::summarise(avg_policy_cost = mean(policy_cost))
} # }
```
