# Get EMPG data

Get EMPG data

## Usage

``` r
get_emergency_management_performance(
  file_path = file.path(get_box_path(), "hazards", "FEMA",
    "emergency-management-performance",
    "emergency_management_performance_grants_2025_06_29.csv"),
  api = TRUE
)
```

## Arguments

- file_path:

  Path to the downloaded dataset on Box.

- api:

  Logical indicating whether to use the OpenFEMA API to retrieve the
  data. Default is TRUE.

## Value

A data frame containing emergency management performance grant (EMPG)
data.
