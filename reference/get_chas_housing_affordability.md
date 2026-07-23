# Obtain housing affordability data from HUD's CHAS

Retrieves HUD Comprehensive Housing Affordability Strategy (CHAS) data,
which cross-tabulate ACS housing-need measures (cost burden,
overcrowding, and related housing problems) by tenure, household income
relative to HUD Area Median Family Income (HAMFI), race/ethnicity, and
household type. Data are drawn either from HUD's CHAS API (for "nation",
"state", "county", "mcd", and "place" geographies) or from an on-disk
parquet mirror on Box (for "tract"). Where a data dictionary is
available, the source's terse column codes are expanded into descriptive
snake_case names (see Details).

## Usage

``` r
get_chas_housing_affordability(
  geography,
  end_year = 2022,
  state_code = NULL,
  entity_code = NULL,
  api = FALSE,
  directory_path = file.path(climateapi::get_box_path(), "built-environment", "hud",
    "comprehensive-housing-affordability-strategies"),
  columns = NULL
)
```

## Arguments

- geography:

  The geographic summary level. One of "nation", "state", "county",
  "mcd" (minor civil division), "place", or "tract". All levels except
  "tract" are served by the API (`api = TRUE`); "tract" is read from
  disk (`api = FALSE`).

- end_year:

  The last year of the five-year ACS period, from 2009 to 2022. Defaults
  to 2022 (the most recent period).

- state_code:

  A two-digit state FIPS code. Required by the API for every geography
  except "nation"; ignored for the disk (tract) path.

- entity_code:

  A FIPS identifier for the requested sub-state geography (county, MCD,
  or place). Required by the API for those geographies; ignored for the
  disk (tract) path.

- api:

  Logical. If `TRUE`, query the HUD API (requires a registered key; see
  [`register_hud_api_key()`](https://ui-research.github.io/climateapi/reference/register_hud_api_key.md)).
  If `FALSE` (the default), read from the on-disk parquet mirror –
  currently only "tract" is available this way.

- directory_path:

  The directory containing the on-disk CHAS parquet files and the data
  dictionary. Defaults to the CHAS folder under the C&C Box path. Used
  for the disk (tract) path and for locating the dictionary used to
  rename columns.

- columns:

  An optional character vector of raw column names to read from the
  parquet file (disk path only). Names refer to the source's original
  column codes, i.e. before the codebook renaming described in Details.
  If `NULL` (the default), all columns are read.

## Value

A tibble of CHAS results, one row per geography. Estimate columns whose
codes appear in the data dictionary are renamed to descriptive
snake_case names of the form `table_<n>_<description>_estimate_<k>`; all
other columns (identifiers such as `geoid`/`tract_geoid`, the `year`
label, and margin-of-error columns) retain their original names. API
results are returned with HUD's own column names unless they happen to
match dictionary codes.

## Details

CHAS releases cover five-year ACS periods; `end_year` is the last year
of that period (e.g. `end_year = 2021` is the 2017-2021 release).

For 2009-2012, the source publishes counts for census tract *parts*
rather than whole tracts; these are summed up to the tract level (keyed
on a `tract_geoid` derived from the source geoid) before being returned.

Column renaming uses the CHAS data dictionary found under
`directory_path` (an `.xlsx` file whose name contains "dictionary",
matched to `end_year`). The dictionary's hierarchical descriptions are
collapsed into a single descriptive snake_case name per column and
applied to any matching columns; columns without a dictionary match
(including margin-of-error columns, which the dictionary does not
describe the same way) keep their original names. If no dictionary is
found (for example, when querying the API without the Box mirror
synced), the data are returned with the source's original column names.

## Examples

``` r
if (FALSE) { # \dontrun{
## tract-level data for the 2017-2021 period, read from the on-disk Box mirror
get_chas_housing_affordability(geography = "tract", end_year = 2021)

## county-level data from the HUD API (requires a registered key)
register_hud_api_key("your-hud-api-key")
get_chas_housing_affordability(geography = "county", end_year = 2021, state_code = "01", entity_code = "001", api = TRUE)
} # }
```
