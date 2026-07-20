# Access county-level data on NFIP policies

Retrieves National Flood Insurance Program (NFIP) policy data at the
county level, including both current and historical policies.

## Usage

``` r
get_nfip_policies(
  state_abbreviation,
  county_geoids = NULL,
  file_name = NULL,
  api = FALSE
)
```

## Arguments

- state_abbreviation:

  A 2 letter state abbreviation (e.g. TX).

- county_geoids:

  A character vector of five-digit county codes.

- file_name:

  The name (not the full path) of a per-state Box file containing the
  raw data (see `@details`). If NULL (default), reads
  `state_abbreviation`'s records directly from the most recently cached
  nationwide file for this dataset (see
  [`get_openfema_cache_path()`](https://ui-research.github.io/climateapi/reference/get_openfema_cache_path.md)),
  which avoids the cross-border duplication issue described below
  entirely.

- api:

  If TRUE, query the API. If FALSE (default), read from `file_name`.

## Value

A dataframe of project-level funding requests and awards, along with
variables that can be aggregated to the county level.

- id:

  Unique identifier for the policy record; use this to de-duplicate
  cross-border policies after combining results from multiple states –
  see `@details`.

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

Data are from FEMA's OpenFEMA API. See
<https://www.fema.gov/openfema-data-page/fima-nfip-redacted-policies-v2>.

The following dataset houses information on NFIP policies (both historic
and current). In order to filter to current policies, the analyst will
need to filter on the policy_date_termination and policy_date_effective
columns.

The dataset also contains both residential and commercial policies. In
order to filter to residential policies, the analyst can filter out the
"non-residential" occupancy type.

When `file_name` is supplied explicitly, per-state files (in the
`intermediate/` Box folder) are not mutually exclusive: a policy whose
county sits near a state border can appear in more than one state's
file. When looping this function over multiple states this way and
combining results, run `dplyr::distinct(id, .keep_all = TRUE)` on the
combined data to remove these duplicates (verified: 72 Delaware-file
rows keyed to Maryland counties, all also present in Maryland's own
file). This does not apply to the default (cache-backed) mode, which
reads directly from the single nationwide file.

## Examples

``` r
if (FALSE) { # \dontrun{
 test <- get_nfip_policies(
      state_abbreviation = "TX",
      county_geoids = c("48201"),
      file_name = "fima_nfip_policies_2025_10_14.parquet",
      api = FALSE) |>
    dplyr::filter(
      !building_occupancy_type %in% c("non-residential"), ### only residential claims,
      policy_date_termination >= as.Date("2025-10-15"),
      policy_date_effective <= as.Date("2025-10-15")) |>
    dplyr::group_by(county_geoid)|>
    dplyr::summarise(avg_policy_cost = mean(policy_cost))
} # }
```
