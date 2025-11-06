# Acquire wildfire perimeters

Acquire wildfire perimeters

## Usage

``` r
get_current_fire_perimeters(
  geography = NULL,
  file_path = NULL,
  bbox = NULL,
  api = TRUE
)
```

## Arguments

- geography:

  Included only for API consistency; this must be NULL.

- file_path:

  Included only for API consistency; this must be NULL.

- bbox:

  Optionally, an sf::st_bbox() object, or an object that can be
  converted to such.

- api:

  Included only for API consistency; this must be TRUE.

## Value

A library(sf) enabled dataframe comprising perimeters of current
wildfires.

## Examples

``` r
if (FALSE) { # \dontrun{
get_current_fire_perimeters()
} # }
```
