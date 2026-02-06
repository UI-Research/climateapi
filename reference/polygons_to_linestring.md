# Convert polygons into their component linestrings

Convert polygons into their component linestrings

## Usage

``` r
polygons_to_linestring(.sf)
```

## Arguments

- .sf:

  The spatial dataframe containing one or more polygons

## Value

An `sf` object (simple feature collection) with geometry type
LINESTRING. The returned object contains:

- polygon_id:

  Integer. The row index of the originating polygon from the input `.sf`
  object, enabling linkage back to the source polygon.

- line_id:

  Integer. A sequential identifier for each line segment within its
  originating polygon. Line segments are ordered according to the vertex
  sequence of the polygon boundary.

- ...:

  All original attributes from the input `.sf` object are preserved and
  joined back via `polygon_id`.

- geometry:

  LINESTRING geometry. Each line segment represents one edge of the
  original polygon boundary.

The CRS of the output matches the input `.sf` object (transformed to
EPSG:5070 during processing).
