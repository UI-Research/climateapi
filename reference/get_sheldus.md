# Access temporal county-level SHELDUS hazard damage data

Retrieves county-level hazard event data from the Spatial Hazard Events
and Losses Database for the United States (SHELDUS), including property
damage, crop damage, fatalities, and injuries.

## Usage

``` r
get_sheldus(
  file_path = file.path(get_box_path(), "hazards", "sheldus",
    "SHELDUS_23.0_12312023_AllCounties_CountyAggregate_YearMonthHazard_2023USD",
    "direct_loss_aggregated_output_24075.csv")
)
```

## Arguments

- file_path:

  The path to the raw SHELDUS data.

## Value

A dataframe comprising hazard x month x year x county observations of
hazard events. Columns include:

- unique_id:

  Unique identifier for each observation.

- GEOID:

  Five-digit county FIPS code.

- state_name:

  Full state name (sentence case).

- county_name:

  County name.

- year:

  Year of the hazard event(s).

- month:

  Month of the hazard event(s).

- hazard:

  Type of hazard (e.g., "Flooding", "Hurricane/Tropical Storm").

- damage_property:

  Property damage in 2023 inflation-adjusted dollars.

- damage_crop:

  Crop damage in 2023 inflation-adjusted dollars.

- fatalities:

  Number of fatalities.

- injuries:

  Number of injuries.

- records:

  Number of individual events aggregated into this observation.

## Details

Data are from Arizona State University's SHELDUS database. Access
requires a subscription. See <https://cemhs.asu.edu/sheldus>.

## Examples

``` r
if (FALSE) { # \dontrun{
get_sheldus()
} # }
```
