# Get Emergency Management Performance Grant (EMPG) data

Retrieves Emergency Management Performance Grant (EMPG) award data from
FEMA, which supports state and local emergency management agencies.

## Usage

``` r
get_emergency_management_performance(file_path = NULL, api = FALSE)
```

## Arguments

- file_path:

  Path to the raw data. If NULL (default), reads the most recently
  cached file for this dataset from
  [`get_openfema_cache_path()`](https://ui-research.github.io/climateapi/reference/get_openfema_cache_path.md).

- api:

  Logical indicating whether to use the OpenFEMA API to retrieve the
  data. Default is FALSE (read from `file_path`, or the local OpenFEMA
  cache if NULL).

## Value

A data frame containing emergency management performance grant (EMPG)
data. Columns include:

- id:

  Unique identifier for the grant record.

- reporting_period:

  The reporting period associated with the record.

- state_name:

  Full state name.

- state_code:

  Two-digit state FIPS code.

- state_abbreviation:

  Two-letter state abbreviation.

- legal_agency_name:

  The name of the legal agency administering the grant.

- project_type:

  The type of project funded.

- year_project_start:

  Year the project started (derived from `project_start_date`, with
  corrections for a handful of records with typos in the raw data).

- project_start_date:

  Date the project started.

- project_end_date:

  Date the project ended.

- name_of_program:

  The name of the EMPG program.

- funding_amount:

  Funding amount in dollars.

## Details

Data are from FEMA's OpenFEMA API. See
<https://www.fema.gov/openfema-data-page/emergency-management-performance-grants-v2>.
