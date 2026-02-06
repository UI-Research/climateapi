# Estimate counts of hazard-impacted structures by structure type

Retrieves building footprint data from the USA Structures dataset and
summarizes structure counts by type at the tract or county level.

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

Depends on the `keep_structures` parameter:

**When `keep_structures = FALSE` (default):** A tibble containing
structure counts aggregated by geography and occupancy type, with
columns:

- GEOID:

  Character. Census geography identifier (county FIPS or tract GEOID
  depending on `geography` parameter).

- primary_occupancy:

  Character. The primary occupancy classification of the structures
  (e.g., "Single Family Dwelling", "Multi - Family Dwelling").

- occupancy_class:

  Character. Broad occupancy classification (e.g., "Residential",
  "Commercial").

- count:

  Integer. Number of structures of this occupancy type in the geography.

**When `keep_structures = TRUE`:** A named list with two elements:

- structures_summarized:

  The aggregated tibble described above.

- structures_raw:

  An `sf` object (POINT geometry) containing individual structure
  records with columns: `unique_id` (building ID), `occupancy_class`,
  `primary_occupancy`, `county_fips`, and geometry.

## Examples

``` r
if (FALSE) { # \dontrun{
get_structures(
  geography = "tract",
  boundaries = tigris::states(cb = TRUE) %>% dplyr::filter(stringr::str_detect(NAME, "District")))
} # }
```
