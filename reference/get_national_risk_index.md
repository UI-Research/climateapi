# Get FEMA National Risk Index scores

Downloads the complete FEMA National Risk Index (NRI) for every US
census tract or county. The NRI characterizes a community's relative
risk from 18 natural hazards by combining expected annual loss, social
vulnerability, and community resilience. All fields published by the NRI
feature service are returned, with the source's abbreviated field codes
expanded into descriptive snake_case column names (see Details).

## Usage

``` r
get_national_risk_index(geography = c("tract", "county"), cache_path = NULL)
```

## Arguments

- geography:

  The geographic summary level of the results. One of "tract" (the
  default) or "county".

- cache_path:

  Optional path to a `.parquet` file used as a read-through cache. If
  supplied and the file already exists, data are read from it and no
  download occurs. If supplied and the file does not exist, freshly
  downloaded data are written there for reuse. If `NULL` (the default),
  data are downloaded fresh and not written to disk. Because the NRI is
  periodically revised (see Details), delete a stale cache file to force
  a refresh.

## Value

A tibble with one row per census tract or county. Every field published
by the NRI service is returned, with the source's abbreviated field
codes expanded into descriptive snake_case names (see Details). Columns
fall into these families:

- Identifiers:

  `geoid` (a standardized, zero-padded 11-digit tract or 5-digit county
  FIPS join key, prepended by this function), the `state_name` and
  `county_name` labels, and the source version `nri_version`. The NRI's
  redundant identifier fields (`nri_id`, the individual state and county
  FIPS codes, and `tractfips`) are dropped in favor of `geoid`.

- Community totals:

  `population`, `value_building` (building value, dollars),
  `value_agriculture` (agricultural value, dollars), and `area_sq_mi`
  (square miles).

- Composite index families:

  `risk_*` (overall National Risk Index), `estimated_annual_loss_*` and
  `expected_annual_loss_rate_composite_*` (expected annual loss and its
  annualized rate), `social_vulnerability_index_*` (social
  vulnerability), `resilience_*` (community resilience), and
  `community_risk_factor_value`. Each family carries some combination of
  `_score`/`_state_percentile`/`_national_percentile` (percentile),
  `_rating` (rating), and `_value`/`_value_*` (absolute) columns.

- Per-hazard metrics:

  One block per hazard, prefixed by the hazard name: `avalanche`,
  `coastal_flood`, `cold_wave`, `drought`, `earthquake`, `hail`,
  `heat_wave`, `hurricane`, `ice_storm`, `landslide`, `lightning`,
  `inland_flood`, `severe_wind`, `tornado`, `tsunami`, `volcano`,
  `wildfire`, and `winter_weather`. Within each block, suffixes denote
  the metric: `_event_count`/`_annual_frequency` (event count and
  annualized frequency), `_exposure_*` and `_exposed_area` (exposure),
  `_historic_loss_ratio_*` (historic loss ratio),
  `_estimated_annual_loss_*` (expected annual loss),
  `_expected_annual_loss_rate_*` (annualized loss rate), and
  `_risk_value`/ `_risk_score`/`_risk_rating` (hazard risk value, score,
  and rating).

Coastal or otherwise geographically limited hazards are `NA` for
communities with no exposure.

## Details

Data are pulled from FEMA's National Risk Index ArcGIS feature services
(one service per geography) and paginated 2,000 records at a time. After
[`janitor::clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.html)
normalization, the NRI's abbreviated field codes are expanded into
readable snake_case names (for example, `WFIR_EALT` becomes
`wildfire_estimated_annual_loss_total`, and `EAL_SPCTL` becomes
`estimated_annual_loss_state_percentile`); the ArcGIS service artifacts
(`OBJECTID`, `Shape__Area`, `Shape__Length`) and redundant source
identifier columns are dropped, and a standardized `geoid` join key is
prepended.

Interpreting the fields depends on the suffix:

- `*_score`, `*_state_percentile`, and `*_national_percentile` columns
  are **values from 0 to 100**: `*_score` and `*_national_percentile`
  rank a community against all US communities of the same type (all
  tracts, or all counties), while `*_state_percentile` ranks it against
  communities in the same state. Because these are ranks rather than
  additive quantities, they **cannot be summed or averaged across
  geographies**; to describe risk for an area spanning several tracts,
  request `geography = "county"` (or higher) directly rather than
  aggregating tract scores.

- `*_rating` columns are categorical labels (e.g. "Relatively High").

- Value and loss columns (`*_estimated_annual_loss_*`, `*_exposure_*`,
  `*_exposed_area`, `*_event_count`, and the community totals
  `value_building`, `value_agriculture`, `population`, and `area_sq_mi`)
  are absolute quantities (dollars, counts, or areas) and *are* additive
  across geographies.

- Rate and ratio columns (`*_annual_frequency`,
  `*_historic_loss_ratio_*`, and `*_expected_annual_loss_rate_*`) are
  normalized rates rather than absolute quantities and, like the
  percentile columns, **cannot be summed across geographies**.

Field definitions are documented in FEMA's NRI Technical Documentation
and data dictionary at <https://hazards.fema.gov/nri/data-resources>
(which uses the original abbreviated field codes). Hazard coverage
reflects NRI v1.20 (December 2025), in which inland flooding (the
`inland_flood_*` columns) replaced the earlier riverine flooding hazard;
the source version is carried in the `nri_version` column. If a future
NRI release changes the schema, the underlying query surfaces the
service error rather than silently truncating results.

## Examples

``` r
if (FALSE) { # \dontrun{
# county-level scores, downloaded fresh
nri_counties <- get_national_risk_index(geography = "county")

# tract-level scores, cached locally for reuse across sessions
nri_tracts <- get_national_risk_index(
  geography = "tract",
  cache_path = file.path(tempdir(), "nri_tracts.parquet"))
} # }
```
