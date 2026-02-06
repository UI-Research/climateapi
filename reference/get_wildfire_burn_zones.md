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

An sf dataframe comprising wildfire burn zone disasters. Each row
represents a single wildfire event, with polygon geometries representing
burn zones. Columns include:

- wildfire_id:

  Unique identifier for the wildfire event.

- id_fema:

  FEMA disaster identifier (if applicable).

- year:

  Year of the wildfire.

- wildfire_name:

  Name of the wildfire or fire complex.

- county_fips:

  Pipe-delimited string of five-digit county FIPS codes for all counties
  affected by the wildfire.

- county_name:

  Pipe-delimited string of county names for all counties affected by the
  wildfire.

- area_sq_km:

  Burned area in square kilometers.

- wildfire_complex_binary:

  Whether the fire is a complex (multiple fires).

- date_start:

  Ignition date.

- date_containment:

  Containment date.

- fatalities_total:

  Total fatalities.

- injuries_total:

  Total injuries.

- structures_destroyed:

  Number of structures destroyed.

- structures_threatened:

  Number of structures threatened.

- evacuation_total:

  Total evacuations.

- wui_type:

  Wildland-urban interface type.

- density_people_sq_km_wildfire_buffer:

  Population density in wildfire buffer area.

- geometry:

  Burn zone polygon geometry.

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
