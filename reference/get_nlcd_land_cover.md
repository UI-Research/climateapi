# Get vectorized Annual NLCD land cover data

Summarizes Annual National Land Cover Database (NLCD) land cover – a
30-meter categorical raster – to Census tracts (by default) or to a set
of user-provided geometries, returning, for each land cover class, both
its share of the geometry's area and its area in square meters.

## Usage

``` r
get_nlcd_land_cover(
  county_geoids = NULL,
  year = 2024,
  geometries = NULL,
  return_raw = FALSE
)
```

## Arguments

- county_geoids:

  A vector of five-character county GEOID(s). Used to fetch tract
  geometries when `geometries` is not supplied; ignored when
  `geometries` is supplied.

- year:

  One or more years for which to retrieve land cover, each between 1985
  and 2024 (the range covered by the Annual NLCD product). Defaults to
  2024, the most recent vintage.

- geometries:

  An `sf`-formatted dataframe specifying the geometries to which NLCD
  land cover should be summarized. Must contain a `GEOID` column and
  have a defined coordinate reference system. Default is `NULL`, in
  which case land cover is summarized to Census tracts for the counties
  named in `county_geoids`.

- return_raw:

  Logical. If `TRUE`, the raw (cropped) NLCD raster(s) returned by
  [`FedData::get_nlcd_annual()`](https://docs.ropensci.org/FedData/reference/get_nlcd_annual.html)
  are attached to the result as an attribute named `"nlcd_raster"`.

## Value

A tibble with one row per geometry, year, and land cover class present
in that geometry:

- GEOID:

  Character. The geometry identifier (tract GEOID, or the `GEOID` of the
  supplied `geometries`).

- year:

  Integer. The NLCD vintage.

- land_cover_code:

  Integer. The NLCD land cover class code (e.g. 11, 21, 42).

- land_cover_class:

  Character. The NLCD class name (e.g. "Open Water", "Deciduous
  Forest").

- area_fraction:

  Numeric. The coverage-weighted share (0-1) of the geometry's own area
  falling in this land cover class. A within-geometry proportion – not
  comparable across geometries on its own; use `area_sq_meters` for
  that.

- area_sq_meters:

  Numeric. The absolute area, in square meters, of this record – i.e. of
  this land cover class within this geometry and year. Comparable across
  geometries. Sum it by `GEOID` (optionally with `year`) to recover a
  geometry's total area.

When `return_raw = TRUE`, the raw NLCD rasters (the tibble returned by
[`FedData::get_nlcd_annual()`](https://docs.ropensci.org/FedData/reference/get_nlcd_annual.html),
carrying a `rast` list-column of `SpatRaster`s) are attached to the
result as the `"nlcd_raster"` attribute.

## Details

Land cover is drawn from the Annual NLCD product (Multi-Resolution Land
Characteristics Consortium). As of the Collection 1.1 release, Annual
NLCD provides a continuous annual time series for the conterminous
United States spanning 1985 through 2024. Because land cover is
categorical, each geometry is summarized as the coverage-weighted
fraction of its area falling in each of the NLCD land cover classes (via
`exactextractr::exact_extract(fun = "frac")`) rather than as a single
average value.

The raster download is delegated to
[`FedData::get_nlcd_annual()`](https://docs.ropensci.org/FedData/reference/get_nlcd_annual.html).
Because the Annual NLCD product is currently published only for the
conterminous US, Alaska, Hawaii, and the territories are unsupported.

## Examples

``` r
if (FALSE) { # \dontrun{
# Land cover for every tract in the District of Columbia, 2024
get_nlcd_land_cover(county_geoids = "11001")

# A multi-year panel over a user-supplied set of geometries
geographies = tigris::tracts(state = "24", county = "031", cb = TRUE) %>%
  dplyr::select(GEOID)
get_nlcd_land_cover(geometries = geographies, year = c(2001, 2012, 2024))
} # }
```
