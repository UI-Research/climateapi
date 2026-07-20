# Get Hazard Mitigation Assistance (HMA) Project Details

Retrieves Hazard Mitigation Assistance project data from both the legacy
HMA Projects dataset and the newer FEMA GO subapplications dataset,
harmonized at the project-county level.

## Usage

``` r
get_hazard_mitigation_assistance(
  file_path_old_grant_system = NULL,
  file_path_new_grant_system = NULL,
  state_abbreviations = NULL
)
```

## Arguments

- file_path_old_grant_system:

  The file path to raw data for HMA applications from the older
  grant-reporting system. These data are typically available from:
  <https://www.fema.gov/openfema-data-page/hazard-mitigation-assistance-projects-v4>
  If NULL (default), reads the most recently cached file for this
  dataset from
  [`get_openfema_cache_path()`](https://ui-research.github.io/climateapi/reference/get_openfema_cache_path.md).

- file_path_new_grant_system:

  The file path to raw data for HMA applications from the newer (FEMA
  GO) grant-reporting system. These data are typically available from:
  <https://www.fema.gov/openfema-data-page/hma-subapplications-v2> If
  NULL (default), reads the most recently cached file for this dataset
  from
  [`get_openfema_cache_path()`](https://ui-research.github.io/climateapi/reference/get_openfema_cache_path.md).

- state_abbreviations:

  NULL by default, in which case data are returned for all 51 states.
  Provide a vector of two-character USPS state abbreviations to obtain
  data for a sub-selection of states.

## Value

A dataframe of project-county HMA application data. Only
`project_cost_federal_split` should be used for county-level
aggregations. Columns include:

- data_source:

  "hma-projects" (legacy) or "hma-subapplications" (FEMA GO).

- project_id:

  Unique project identifier.

- disaster_number:

  FEMA disaster number (if disaster-related).

- project_program_area:

  HMA program: HMGP, BRIC, FMA, or PDM.

- project_fiscal_year:

  Fiscal year of the project.

- state_name:

  Full state name.

- county_geoid:

  Five-digit county FIPS code.

- county_population:

  County population used for allocation.

- project_status:

  Current project status (e.g., "Closed", "Active") – spans the full
  application lifecycle; see `@details`.

- project_cost_federal:

  Total federal cost at project level.

- project_cost_federal_split:

  Federal cost allocated to this county.

## Details

Data are from FEMA's OpenFEMA API, combining two data sources: the
legacy Hazard Mitigation Assistance Projects (v4) and the newer HMA
Subapplications (v2). Multi-county projects are split across counties
based on population proportions.

Records span the full application lifecycle, not only approved/funded
projects – this includes Denied, Withdrawn, Void, Not Selected, and
other pre-decision statuses (the FEMA GO source in particular contains
no funded/"Selected" status records at all). Users must filter on
`project_status` themselves before summing `project_cost_federal_split`
if only approved or funded projects are of interest.

## Examples

``` r
if (FALSE) { # \dontrun{
get_hazard_mitigation_assistance()
} # }
```
