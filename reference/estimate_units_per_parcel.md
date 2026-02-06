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

An `sf` object (point geometry, representing parcel centroids)
containing the input parcel data augmented with estimated residential
unit information. The returned object includes:

- parcel_id:

  Character or numeric. The unique parcel identifier from the input
  data.

- tract_geoid:

  Character. The 11-digit Census tract GEOID containing the parcel
  centroid.

- jurisdiction:

  Character. The jurisdiction name associated with the parcel.

- municipality_name:

  Character. The municipality name associated with the parcel.

- residential_unit_count:

  Numeric. The estimated number of residential units on the parcel,
  benchmarked against ACS estimates at the tract level.

- residential_unit_categories:

  Factor (ordered). Categorical classification of unit counts: "0", "1",
  "2", "3-4", "5-9", "10-19", "20-49", "50+".

- median_value_improvement_sf:

  Numeric. Tract-level median improvement value for single-family
  parcels.

- median_value_improvement_mh:

  Numeric. Tract-level median improvement value for manufactured home
  parcels.

- acs_units_N:

  Numeric columns (acs_units_1, acs_units_2, etc.). ACS-reported housing
  unit counts by units-in-structure category for the tract.

- zone:

  Character. Zoning designation from the zoning dataset.

- zoned_housing_type:

  Character. Housing type allowed by zoning.

- far:

  Numeric. Floor area ratio.

- setback_front:

  Numeric. Front setback requirement in feet.

- setback_rear:

  Numeric. Rear setback requirement in feet.

- setback_side:

  Numeric. Side setback requirement in feet.

- height_maximum:

  Numeric. Maximum building height allowed.
