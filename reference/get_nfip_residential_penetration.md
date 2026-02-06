# Get the share of residential structures covered by NFIP

Get the share of residential structures covered by NFIP

## Usage

``` r
get_nfip_residential_penetration(
  states = NULL,
  file_name = "nfip_residential_penetration_rates_12_12_2025.csv"
)
```

## Arguments

- states:

  NULL by default.

- file_name:

  The name (not full path) of the raw dataset.

## Value

A tibble with the following columns:

- state_name:

  State name

- couty_name:

  County name

- GEOID:

  Five digits county FIPS code

- year_data:

  The year of the data

- penetration_rate:

  Share of all residential structures insured by NFIP

- penetration_rate_sfha:

  Share of all residential structures in the Special Flood Hazard Area
  insured by NFIP

- contracts_in_force:

  Residential NFIP contracts currently in effect ("in force")

- contracts_in_force_sfha:

  Residential NFIP contracts in the Special Flood Hazard Area currently
  in effect ("in force")

- residential_structures:

  The number of residential structures, derived from the 2022 National
  Structure Inventory

- residential_structures_sfha:

  The number of residential structures in the Special Flood Hazard Area,
  derived from the 2022 National Structure Inventory

## Examples

``` r
if (FALSE) { # \dontrun{
get_nfip_residential_penetration()
} # }
```
