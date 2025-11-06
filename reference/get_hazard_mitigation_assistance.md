# Get Hazard Mitigation Assistance (HMA) Project Details

Get Hazard Mitigation Assistance (HMA) Project Details

## Usage

``` r
get_hazard_mitigation_assistance(
  file_path_old_grant_system = file.path(get_box_path(), "hazards", "fema",
    "hazard-mitigation-assistance", "raw",
    "HazardMitigationAssistanceProjects_2025_09_27.parquet"),
  file_path_new_grant_system = file.path(get_box_path(), "hazards", "fema",
    "hazard-mitigation-assistance", "raw", "HmaSubapplications_2025_09_27.parquet"),
  state_abbreviations = NULL
)
```

## Arguments

- file_path_old_grant_system:

  The file path to raw data for HMA applications from the older
  grant-reporting system. These data are typically available from:
  https://www.fema.gov/openfema-data-page/hazard-mitigation-assistance-projects-v4

- file_path_new_grant_system:

  The file path to raw data for HMA applications from the newer (FEMA
  GO) grant-reporting system. These data are typically available from:
  https://www.fema.gov/openfema-data-page/hma-subapplications-v2

- state_abbreviations:

  NULL by default, in which case data are returned for all 51 states.
  Provide a vector of two-character USPS state abbreviations to obtain
  data for a sub-selection of states.

## Value

A dataframe of project-county HMA application data, aggregated across
both old and new grant reporting systems.

## Examples

``` r
if (FALSE) { # \dontrun{
get_hazard_mitigation_assistance()
} # }
```
