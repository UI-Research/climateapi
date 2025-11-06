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

A dataframe (optionally, an sf-dataframe) comprising Census geographies
