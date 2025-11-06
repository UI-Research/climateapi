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

A simple feature collection of linestrings derived from the inputted
polygons; all attributes are retained, and two new
attributes–`polygon_id` and `line_id`–are prepended to the output
