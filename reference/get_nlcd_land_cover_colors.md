# Get NLCD land cover classes and their standard colors

Returns the Annual National Land Cover Database (NLCD) land cover
classification as a lookup table: each land cover class, its numeric
code, the standard NLCD display color (as a hex code), and a short
description. This is the same class-and-color reference that
[`get_nlcd_land_cover()`](https://ui-research.github.io/climateapi/reference/get_nlcd_land_cover.md)
uses internally, exposed so that you can label and color maps or charts
by land cover class without depending on the FedData package directly.

## Usage

``` r
get_nlcd_land_cover_colors()
```

## Value

A tibble with one row per NLCD land cover class:

- land_cover_code:

  Integer. The NLCD land cover class code (e.g. 11, 21, 42).

- land_cover_class:

  Character. The NLCD class name (e.g. "Open Water", "Deciduous
  Forest").

- color:

  Character. The standard NLCD display color for the class, as a hex
  code (e.g. "#5475A8").

- description:

  Character. A short description of the land cover class.

## Examples

``` r
if (FALSE) { # \dontrun{
# the class-to-color lookup used to color NLCD land cover maps
get_nlcd_land_cover_colors()
} # }
```
