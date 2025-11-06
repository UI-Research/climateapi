# Estimate the number and types of structures per parcel

Estimate the number and types of structures per parcel

## Usage

``` r
estimate_units_per_parcel(structures, parcels, zoning, acs = NULL)
```

## Arguments

- structures:

  A dataset returned by `get_structure()`.

- parcels:

  A spatial (polygon) dataset.

- zoning:

  A spatial (polygon) zoning dataset.

- acs:

  Optionally, a non-spatial dataset, at the tract level, returned from
  [`urbnindicators::compile_acs_data()`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.html).

## Value

The inputted parcels datasets with attributes describing estimated unit
counts by unit type.
