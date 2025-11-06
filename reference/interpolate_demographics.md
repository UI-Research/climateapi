# Interpolate tract-level sociodemographic data to zoning polygons

Interpolate tract-level sociodemographic data to zoning polygons

## Usage

``` r
interpolate_demographics(
  zones_sf,
  sociodemographic_tracts_sf = NULL,
  id_column,
  weights = "population"
)
```

## Arguments

- zones_sf:

  A spatial (sf) dataset defining zoning districts.

- sociodemographic_tracts_sf:

  (optional) A tract level spatial (sf) dataset resulting from
  urbnindicators. If NULL, this function is run behind the scenes for
  appropriate tracts.

- id_column:

  The name of the column in zones_sf that identifies each unique
  combination of zoning regulations.

- weights:

  One of c("population", "housing"). The variable to be used as the
  weight in the interpolation.

## Value

A spatial (sf) dataset comprising one observation for each level of
id_column with interpolated values taken from
sociodemographic_tracts_sf.
