# Get Emergency Management Performance Grant (EMPG) data

Retrieves Emergency Management Performance Grant (EMPG) award data from
FEMA, which supports state and local emergency management agencies.

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
data. Columns include:

- id:

  Unique identifier for the grant record.

- state_name:

  Full state name.

- state_code:

  Two-digit state FIPS code.

- state_abbreviation:

  Two-letter state abbreviation.

- year_project_start:

  Year the project started.

- project_start_date:

  Date the project started.

- project_end_date:

  Date the project ended.

- grant_amount:

  Total grant amount in dollars.

- federal_share:

  Federal portion of the grant in dollars.

- non_federal_share:

  Non-federal cost share in dollars.

- program:

  EMPG program type.

## Details

Data are from FEMA's OpenFEMA API. See
<https://www.fema.gov/openfema-data-page/emergency-management-performance-grants-v2>.
