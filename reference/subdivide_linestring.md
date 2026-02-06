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

An `sf` object (simple feature collection) with geometry type
LINESTRING. The returned object contains:

- row_id:

  Integer. The row index from the original input linestring, allowing
  linkage back to the input data.

- ...:

  All original attributes from the input `line` object are preserved and
  joined back via `row_id`.

- geometry:

  LINESTRING geometry. Each segment is at most `max_length` units long
  (in the CRS units). Segments shorter than `max_length` in the input
  are returned unchanged.

The CRS of the output is set to the value specified by the `crs`
parameter (default: EPSG:5070).
