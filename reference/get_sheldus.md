# Access temporal county-level SHELDUS hazard damage data

Retrieves county-level hazard event data from the Spatial Hazard Events
and Losses Database for the United States (SHELDUS), including property
damage, crop damage, fatalities, and injuries.

## Usage

``` r
get_sheldus(file_path = NULL)
```

## Arguments

- file_path:

  The path to the raw SHELDUS data. If NULL (default), the most recently
  modified `direct_loss_aggregated_output_*.csv` file found under Box's
  `hazards/sheldus` directory is used, and a message discloses which
  file/vintage was selected.

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

  Property damage in inflation-adjusted dollars (base year varies by
  SHELDUS vintage; see the function's message for the base year used).

- damage_crop:

  Crop damage in inflation-adjusted dollars (see `damage_property`).

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
