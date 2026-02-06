# Get Disaster Dollar Database Data

Get Disaster Dollar Database Data

## Usage

``` r
get_disaster_dollar_database(
  file_path = file.path(get_box_path(), "hazards", "carnegie-endowment",
    "disaster_dollar_database_2025_11_19.csv")
)
```

## Arguments

- file_path:

  The path (on Box) to the file containing the raw data.

## Value

A dataframe comprising disaster-level observations with financial
assistance metrics from FEMA's Individual and Households Program (IHP),
Public Assistance (PA), HUD's Community Development Block Grant Disaster
Recovery (CDBG-DR), and SBA disaster loans.

## Details

These data are sourced from:
https://carnegieendowment.org/features/disaster-dollar-database. The
data returned from this function are unchanged, though some columns have
been renamed slightly for clarity and consistency.

## Examples

``` r
if (FALSE) { # \dontrun{
get_disaster_dollar_database()
} # }
```
