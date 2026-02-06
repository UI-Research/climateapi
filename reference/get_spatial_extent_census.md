# Get the Census geographies that overlap with the input spatial dataset

Get the Census geographies that overlap with the input spatial dataset

## Usage

``` r
get_spatial_extent_census(data, return_geometry = FALSE, projection = 5070)
```

## Arguments

- data:

  An sf-formatted dataframe

- return_geometry:

  Logical. Include the geometries of returned geographies?

- projection:

  The EPSG code of the desired projection. Default is 5070 (Albers Equal
  Area).

## Value

A tibble (or `sf` object if `return_geometry = TRUE`) containing Census
geographies that overlap with the input spatial data. The structure
depends on the geographic extent:

- When multiple states overlap:

  Returns state-level data with columns: `state_geoid` (2-digit FIPS),
  `geography` ("state").

- When a single state overlaps:

  Returns tract-level data with columns: `state_geoid` (2-digit FIPS),
  `county_geoid` (5-digit FIPS), `geography` ("tract").

If `return_geometry = TRUE`, the geometry column is retained; otherwise
it is dropped.
