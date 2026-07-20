# Get wildfire burn zones

Returns spatial data on wildfire burn zones in the US from 2000-2025.
This dataset harmonizes six wildfire datasets (FIRED, MTBS, NIFC,
ICS-209, RedBook, and FEMA) to identify wildfires that burned near
communities and resulted in civilian fatalities, destroyed structures,
or received federal disaster relief.

## Usage

``` r
get_wildfire_burn_zones(
  file_path = file.path(get_box_path(), "hazards", "other-sources",
    "wildfire-burn-zones", "wfbz_disasters_2000-2025.geojson")
)
```

## Arguments

- file_path:

  The path to the geojson file containing the raw data. Defaults to a
  path within Box.

## Value

An sf dataframe comprising wildfire burn zone disasters, with one row
per wildfire x affected county (a wildfire spanning multiple counties
appears as multiple rows). `geometry`, `area_sq_km`, and the other
wildfire-level summary columns (`fatalities_total`, `injuries_total`,
`structures_destroyed`, `structures_threatened`, `evacuation_total`,
`wui_type`, `density_people_sq_km_wildfire_buffer`) are wildfire-level
and repeat identically across a multi-county wildfire's rows;
de-duplicate on `wildfire_id` before summing these columns or unioning
geometries (e.g. `sum(area_sq_km[!duplicated(wildfire_id)])`) to avoid
over-counting. Columns include:

- wildfire_id:

  Unique identifier for the wildfire event.

- id_fema:

  FEMA disaster identifier (if applicable).

- year:

  Year of the wildfire.

- wildfire_name:

  Name of the wildfire or fire complex.

- state_fips:

  Two-digit state FIPS code, derived from `county_fips`.

- county_fips:

  Five-digit county FIPS code for a single county affected by the
  wildfire.

- county_name:

  Name of a single county affected by the wildfire, sourced from
  Census's own canonical county names (joined on `county_fips`) rather
  than the raw source data, to avoid mangling Mc-prefixed county names.

- area_sq_km:

  Burned area in square kilometers (wildfire-level; see above).

- wildfire_complex_binary:

  Whether the fire is a complex (multiple fires).

- date_start:

  Ignition date.

- date_containment:

  Containment date.

- fatalities_total:

  Total fatalities (wildfire-level; see above).

- injuries_total:

  Total injuries (wildfire-level; see above).

- structures_destroyed:

  Number of structures destroyed (wildfire-level; see above).

- structures_threatened:

  Number of structures threatened (wildfire-level; see above).

- evacuation_total:

  Total evacuations (wildfire-level; see above).

- wui_type:

  Wildland-urban interface type (wildfire-level; see above).

- density_people_sq_km_wildfire_buffer:

  Population density in wildfire buffer area (wildfire-level; see
  above).

- geometry:

  Burn zone polygon geometry (wildfire-level; see above).

## Details

Data are from a harmonized wildfire burn zone disaster dataset combining
FIRED, MTBS, NIFC, ICS-209, RedBook, and FEMA data sources. Geometries
are in NAD83 / Conus Albers (EPSG:5070).

## Examples

``` r
if (FALSE) { # \dontrun{
burn_zones <- get_wildfire_burn_zones()
} # }
```
