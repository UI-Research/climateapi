# Estimate counts of hazard-impacted structures by structure type

Estimate counts of hazard-impacted structures by structure type

## Usage

``` r
get_structures(boundaries, geography = "county", keep_structures = FALSE)
```

## Arguments

- boundaries:

  A POLYGON or MULTIPOLYGON object, or an sf::st_bbox()-style bbox.

- geography:

  The desired geography of the results. One of "tract" or "county".

- keep_structures:

  Logical. If TRUE, the raw structure data will be returned alongside
  the summarized data.

## Value

A dataframe comprising estimated counts of each structure type, at the
specified `geography`, for all such geographic units intersecting the
`boundaries` object. If keep_structure = TRUE, returns a list with two
elements: the summarized data and the raw structure data.

## Examples

``` r
if (FALSE) { # \dontrun{
get_structures(
  geography = "tract",
  boundaries = tigris::states(cb = TRUE) %>% dplyr::filter(stringr::str_detect(NAME, "District")))
} # }
```
