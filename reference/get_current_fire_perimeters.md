# Acquire current wildfire perimeters

Retrieves current wildfire perimeter data from the NIFC (National
Interagency Fire Center) via the Wildland Fire Interagency Geospatial
Services (WFIGS) API.

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

An sf dataframe comprising perimeters of current wildfires. Columns
include:

- unique_id:

  Unique identifier for each observation (generated).

- incident_name:

  Name of the fire incident (title case).

- incident_size_acres:

  Size of the fire in acres.

- incident_short_description:

  Brief description of the incident.

- percent_contained:

  Percent of fire contained (0-100).

- identified_date:

  Date/time the fire was discovered.

- updated_date:

  Date/time the record was last updated.

- geometry:

  Polygon geometry of the fire perimeter.

## Details

Data are from the NIFC WFIGS service. See
<https://data-nifc.opendata.arcgis.com/datasets/nifc::wfigs-interagency-fire-perimeters/about>.

## Examples

``` r
if (FALSE) { # \dontrun{
get_current_fire_perimeters()
} # }
```
