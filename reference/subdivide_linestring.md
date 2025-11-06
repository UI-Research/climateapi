# Subdivide a linestring into segments of a specified length

Subdivide a linestring into segments of a specified length

## Usage

``` r
subdivide_linestring(line, max_length, crs = 5070)
```

## Arguments

- line:

  A linestring or simple feature collection thereof

- max_length:

  The maximum length of each segment. Segments longer than this value
  will be subdivided; those that are below this threshold will be
  returned as-is.

- crs:

  The coordinate reference system to which the linestring should be
  transformed. Default is 5070.

## Value

A spatial dataframe comprising linestrings below the `max_length`
threshold, linked back to their input linestrings via a `line_id`
attribute
