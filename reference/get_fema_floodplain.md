# Acquire FEMA floodplain boundaries

Retrieves 100-year and 500-year floodplain polygons from FEMA's National
Flood Hazard Layer (NFHL) via its ArcGIS REST API. When FEMA's service
is unavailable, falls back to an Esri-hosted copy of the same layer.

## Usage

``` r
get_fema_floodplain(
  bbox,
  floodplains = c("100-year", "500-year"),
  silent = FALSE
)
```

## Arguments

- bbox:

  An sf::st_bbox() object, or an object that can be converted to such
  (for example, an sf dataframe). Required: the NFHL is a national
  dataset far too large to download in full, so results are limited to
  flood zones that intersect this bounding box. Coordinates are assumed
  to be in the coordinate reference system of the supplied object
  (EPSG:4326 if the object has no CRS).

- floodplains:

  Which floodplain categories to return. One or both of "100-year" and
  "500-year". Defaults to both.

- silent:

  Logical. When FALSE (the default), a message describing the returned
  data is printed, as is a message when the Esri fallback is used and a
  warning when no polygons match. When TRUE, all messages and warnings
  are suppressed; only an error (if the data cannot be obtained) is
  raised.

## Value

An sf dataframe comprising floodplain polygons. Columns include:

- floodplain:

  Floodplain category: "100-year" or "500-year".

- flood_zone:

  The FEMA flood zone designation (e.g., "AE", "VE", "X").

- flood_zone_subtype:

  Additional zone detail (e.g., "FLOODWAY", "0.2 PERCENT ANNUAL CHANCE
  FLOOD HAZARD").

- is_special_flood_hazard_area:

  Logical. TRUE when the polygon is part of the Special Flood Hazard
  Area (the 100-year floodplain).

- static_base_flood_elevation_feet:

  The static base flood elevation, in feet, where one applies (NA
  otherwise).

- flood_insurance_rate_map_panel_id:

  The identifier of the source flood insurance rate map panel.

- geometry:

  Polygon geometry of the flood zone, in EPSG:4326.

## Details

Data are from the National Flood Hazard Layer's "Flood Hazard Zones"
layer (layer 28). See
<https://hazards.fema.gov/arcgis/rest/services/public/NFHL/MapServer/28>.

The 100-year floodplain (more precisely, areas with a one percent or
greater annual chance of flooding, also called the Special Flood Hazard
Area) comprises zones A, AE, AH, AO, AR, A99, V, VE, and VO. The
500-year floodplain (areas with between a 0.2 percent and one percent
annual chance of flooding) comprises the portions of zone X whose zone
subtype begins with "0.2", which covers all of the wordings FEMA uses
for the 0.2 percent annual chance flood hazard, including the variants
for flooding contained in a channel or structure and for coastal zones.
Areas of minimal flood hazard (the remainder of zone X), open water, and
unmapped areas (zone D) are not returned.

Because large requests can overwhelm the NFHL service, the query
proceeds in two steps: it first retrieves the identifiers of all
matching polygons, then downloads their geometries in batches, retrying
each request up to three times.

FEMA's service is the primary source. When it cannot be reached, the
function falls back to an Esri-hosted copy of the same FEMA layer, at
<https://services5.arcgis.com/7weheFjxuNkGGiZi/arcgis/rest/services/USA_Flood_Hazard_Areas_view/FeatureServer/0>.
Esri updates that copy annually, so it can be less current than FEMA's
own service. A message reports whenever the fallback is used. The two
sources write their zone subtype values differently (FEMA uses upper
case and abbreviates "percent" as "PCT"; Esri uses title case and spells
out "Percent"), so values from both are converted to a single form:
upper case, with "PCT" spelled out as "PERCENT".

Note that the NFHL only covers communities with effective digital flood
insurance rate maps; areas without digital maps return no polygons even
though they may face flood risk.

## Examples

``` r
if (FALSE) { # \dontrun{
bbox = sf::st_bbox(
  c(xmin = -77.05, ymin = 38.87, xmax = -77.00, ymax = 38.91),
  crs = 4326)
get_fema_floodplain(bbox = bbox)
get_fema_floodplain(bbox = bbox, floodplains = "100-year")
} # }
```
