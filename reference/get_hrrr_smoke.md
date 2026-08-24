# Get hourly wildfire smoke concentrations from the HRRR-Smoke model

Retrieves hourly near-surface wildfire smoke concentrations (micrograms
per cubic meter) from NOAA's High-Resolution Rapid Refresh (HRRR) model,
cropped to an area of interest, and returns them as a single multi-layer
raster (one layer per hour).

## Usage

``` r
get_hrrr_smoke(
  geometries,
  start_date,
  end_date = start_date,
  variable = c("surface", "column"),
  hours = 0:23
)
```

## Arguments

- geometries:

  An `sf`-formatted dataframe (or an `sfc` geometry column) defining the
  area of interest, in any defined coordinate reference system. The
  returned raster is cropped to this area's bounding box.

- start_date:

  The first day to retrieve, as a `Date` or a "YYYY-MM-DD" string.

- end_date:

  The last day to retrieve (inclusive), as a `Date` or a "YYYY-MM-DD"
  string. Defaults to `start_date`. HRRR-Smoke is archived from 2021
  onward; the most recent hours may not yet be posted.

- variable:

  Which smoke quantity to retrieve: `"surface"` (default; near-surface
  concentration) or `"column"` (vertically integrated smoke). See
  Details.

- hours:

  Which hours of each day (UTC, 0-23) to retrieve. Defaults to all 24;
  for lighter-temporal-weight coverage, pass e.g. `seq(0, 21, by = 3)`.

## Value

A
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
with one layer per successfully retrieved hour, cropped to the bounding
box of `geometries` (buffered by one 3-kilometer cell). Hours missing
from the archive are dropped with a single summary warning. The raster's
components:

- cell values:

  Numeric. The smoke quantity selected by `variable`: near-surface smoke
  concentration in micrograms per cubic meter (ug/m^3) when
  `variable = "surface"`, or vertically integrated column smoke in
  milligrams per square meter (mg/m^2) when `variable = "column"`.

- layers:

  One layer per hour, in chronological order. Convert to a
  one-row-per-cell-per-hour tibble with
  `terra::as.data.frame(x, xy = TRUE, wide = FALSE)`.

- layer names:

  Character. The layer's timestamp in UTC, formatted "YYYY-MM-DD HH:00"
  (e.g. "2025-08-01 12:00").

- time:

  POSIXct. The same UTC timestamps, retrievable with
  [`terra::time()`](https://rspatial.github.io/terra/reference/time.html);
  used directly by `tidyterra` and
  [`terra::animate()`](https://rspatial.github.io/terra/reference/animate.html).

- coordinate reference system:

  The HRRR model's native projection (Lambert conformal conic), with
  3-kilometer cells. Reproject with
  [`terra::project()`](https://rspatial.github.io/terra/reference/project.html),
  or transform vector layers to it with
  `sf::st_transform(x, sf::st_crs(raster))` before mapping.

## Details

HRRR is NOAA's 3-kilometer, hourly-updating weather model for the
conterminous United States. Since late 2020 it has carried smoke as a
modeled quantity, driven by satellite detections of active fires. This
function returns the "analysis" field for each requested hour – the
model's real-time estimate for the hour it was issued. Data are
downloaded on demand from NOAA's free public archive. A two-week window
at hourly resolution takes roughly a few minutes.

Two smoke quantities are available via `variable`:

- `"surface"`:

  Smoke mass density 8 meters above ground, in micrograms per cubic
  meter (ug/m^3). This approximates what people at ground level are
  breathing and is directly comparable to PM2.5 air quality readings,
  which use the same unit. For reference, the EPA's 24-hour PM2.5
  standard is 35 ug/m^3.

- `"column"`:

  Vertically integrated smoke – all smoke in the atmospheric column
  above each cell – in milligrams per square meter (mg/m^2). This
  corresponds to what satellites see and includes high-altitude smoke
  that may never reach the ground.

Because HRRR covers only the conterminous United States, Alaska, Hawaii,
and the territories are unsupported. Note also that these are model
estimates, not directly-measure smoke concentration observations.

## Examples

``` r
if (FALSE) { # \dontrun{
county = tigris::counties(state = "CA", cb = TRUE) %>%
  dplyr::filter(NAME == "Butte")

smoke = get_hrrr_smoke(
  geometries = county,
  start_date = "2025-07-20",
  end_date = "2025-08-03")

# quick look at one hour, and a simple animation across all hours
terra::plot(smoke[[1]])
terra::animate(smoke, pause = 0.1)
} # }
```
