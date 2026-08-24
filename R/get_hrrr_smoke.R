#' Get hourly wildfire smoke concentrations from the HRRR-Smoke model
#'
#' @description
#' Retrieves hourly near-surface wildfire smoke concentrations (micrograms per
#' cubic meter) from NOAA's High-Resolution Rapid Refresh (HRRR) model, cropped
#' to an area of interest, and returns them as a single multi-layer raster
#' (one layer per hour).
#'
#' @details
#' HRRR is NOAA's 3-kilometer, hourly-updating weather model for the
#' conterminous United States. Since late 2020 it has carried smoke as a
#' modeled quantity, driven by satellite detections of active fires. This
#' function returns the "analysis" field for each requested hour -- the
#' model's real-time estimate for the hour it was issued. Data
#' are downloaded on demand from NOAA's free public archive. 
#' A two-week window at hourly resolution takes roughly a few minutes.
#'
#' Two smoke quantities are available via `variable`:
#' \describe{
#'   \item{`"surface"`}{Smoke mass density 8 meters above ground, in
#'     micrograms per cubic meter (ug/m^3). This approximates what people at
#'     ground level are breathing and is directly comparable to PM2.5 air
#'     quality readings, which use the same unit. For reference, the EPA's
#'     24-hour PM2.5 standard is 35 ug/m^3.}
#'   \item{`"column"`}{Vertically integrated smoke -- all smoke in the
#'     atmospheric column above each cell -- in milligrams per square meter
#'     (mg/m^2). This corresponds to what satellites see and includes
#'     high-altitude smoke that may never reach the ground.}
#' }
#'
#' Because HRRR covers only the conterminous United States, Alaska, Hawaii,
#' and the territories are unsupported. Note also that these are model
#' estimates, not directly-measure smoke concentration observations.
#'
#' @param geometries An `sf`-formatted dataframe (or an `sfc` geometry column)
#'   defining the area of interest, in any defined coordinate reference
#'   system. The returned raster is cropped to this area's bounding box.
#' @param start_date The first day to retrieve, as a `Date` or a
#'   "YYYY-MM-DD" string.
#' @param end_date The last day to retrieve (inclusive), as a `Date` or a
#'   "YYYY-MM-DD" string. Defaults to `start_date`. HRRR-Smoke is archived
#'   from 2021 onward; the most recent hours may not yet be posted.
#' @param variable Which smoke quantity to retrieve: `"surface"` (default;
#'   near-surface concentration) or `"column"` (vertically integrated smoke).
#'   See Details.
#' @param hours Which hours of each day (UTC, 0-23) to retrieve. Defaults to
#'   all 24; for lighter-temporal-weight coverage, pass e.g. `seq(0, 21, by = 3)`.
#'
#' @return A `terra::SpatRaster` with one layer per successfully retrieved
#'   hour, cropped to the bounding box of `geometries` (buffered by one
#'   3-kilometer cell). Hours missing from the archive are dropped with a
#'   single summary warning. The raster's components:
#' \describe{
#'   \item{cell values}{Numeric. The smoke quantity selected by `variable`:
#'     near-surface smoke concentration in micrograms per cubic meter
#'     (ug/m^3) when `variable = "surface"`, or vertically integrated
#'     column smoke in milligrams per square meter (mg/m^2) when
#'     `variable = "column"`.}
#'   \item{layers}{One layer per hour, in chronological order. Convert to a
#'     one-row-per-cell-per-hour tibble with
#'     `terra::as.data.frame(x, xy = TRUE, wide = FALSE)`.}
#'   \item{layer names}{Character. The layer's timestamp in UTC, formatted
#'     "YYYY-MM-DD HH:00" (e.g. "2025-08-01 12:00").}
#'   \item{time}{POSIXct. The same UTC timestamps, retrievable with
#'     `terra::time()`; used directly by `tidyterra` and
#'     `terra::animate()`.}
#'   \item{coordinate reference system}{The HRRR model's native projection
#'     (Lambert conformal conic), with 3-kilometer cells. Reproject with
#'     `terra::project()`, or transform vector layers to it with
#'     `sf::st_transform(x, sf::st_crs(raster))` before mapping.}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' county = tigris::counties(state = "CA", cb = TRUE) %>%
#'   dplyr::filter(NAME == "Butte")
#'
#' smoke = get_hrrr_smoke(
#'   geometries = county,
#'   start_date = "2025-07-20",
#'   end_date = "2025-08-03")
#'
#' # quick look at one hour, and a simple animation across all hours
#' terra::plot(smoke[[1]])
#' terra::animate(smoke, pause = 0.1)
#' }
get_hrrr_smoke = function(
    geometries,
    start_date,
    end_date = start_date,
    variable = c("surface", "column"),
    hours = 0:23) {

  variable = match.arg(variable)

  start_date = as.Date(start_date)
  end_date = as.Date(end_date)
  if (is.na(start_date) || is.na(end_date)) {
    stop("`start_date` and `end_date` must be Dates or 'YYYY-MM-DD' strings.") }
  if (end_date < start_date) {
    stop("`end_date` must not be earlier than `start_date`.") }
  ## HRRR added smoke fields with the model's version 4 upgrade in December 2020;
  ## the AWS archive holds them reliably from 2021 onward
  if (start_date < as.Date("2021-01-01")) {
    stop("HRRR-Smoke fields are available in the archive from 2021-01-01 onward.") }
  if (!all(hours %in% 0:23)) {
    stop("`hours` must contain only integers between 0 and 23.") }

  if (inherits(geometries, "sfc")) { geometries = sf::st_as_sf(geometries) }
  if (!inherits(geometries, "sf")) {
    stop("`geometries` must be a simple features (sf) object.") }
  if (is.na(sf::st_crs(geometries))) {
    stop("`geometries` must have a defined coordinate reference system (CRS).") }

  ## the string that identifies the smoke field on a line of the .idx sidecar
  ## file, e.g. "MASSDEN:8 m above ground" (GRIB shorthand for smoke mass density)
  idx_pattern = switch(
    variable,
    surface = "MASSDEN:8 m above ground",
    column = "COLMD:entire atmosphere")

  base_url = "https://noaa-hrrr-bdp-pds.s3.amazonaws.com"

  ## one row per requested hour: the archive path of that hour's analysis file
  requests = tidyr::expand_grid(
      date = seq(start_date, end_date, by = "day"),
      hour = sort(unique(as.integer(hours)))) %>%
    dplyr::mutate(
      timestamp = as.POSIXct(
        stringr::str_c(date, " ", hour, ":00"), tz = "UTC"),
      grib_url = stringr::str_c(
        base_url, "/hrrr.", format(date, "%Y%m%d"), "/conus/hrrr.t",
        sprintf("%02d", hour), "z.wrfsfcf00.grib2"))

  ## fetch one hour's smoke field: read the .idx to find the field's byte range,
  ## download only those bytes, read as a raster. Returns NULL if the hour is
  ## not (yet) in the archive.
  fetch_hour = function(grib_url, timestamp) {
    idx_lines = tryCatch(
      readLines(stringr::str_c(grib_url, ".idx"), warn = FALSE),
      error = function(e) NULL)
    if (is.null(idx_lines)) { return(NULL) }

    ## .idx lines look like "37:24296434:d=2025072012:MASSDEN:8 m above ground:anl:"
    ## -- field 2 is the field's starting byte; the next line's start is its end
    line_number = stringr::str_which(idx_lines, stringr::fixed(idx_pattern))
    if (length(line_number) != 1) { return(NULL) }

    byte_starts = as.numeric(stringr::str_split_i(idx_lines, ":", 2))
    range_start = byte_starts[line_number]
    ## for the last field in the file there is no next line; curl accepts an
    ## open-ended range ("start-"), which reads through the end of the file
    range_end = dplyr::if_else(
      line_number < length(idx_lines),
      as.character(byte_starts[line_number + 1] - 1),
      "")

    grib_file = tempfile(fileext = ".grib2")
    fetch = tryCatch({
        curl::curl_download(
          grib_url,
          grib_file,
          handle = curl::new_handle(
            range = stringr::str_c(range_start, "-", range_end)))
        terra::rast(grib_file) },
      error = function(e) NULL)
    if (is.null(fetch)) { return(NULL) }

    names(fetch) = format(timestamp, "%Y-%m-%d %H:00")
    terra::time(fetch) = timestamp
    fetch
  }

  hourly_rasters = purrr::map2(
    requests$grib_url,
    requests$timestamp,
    fetch_hour) %>%
    purrr::compact()

  if (length(hourly_rasters) == 0) {
    stop(
      "No HRRR-Smoke fields could be retrieved for the requested window. ",
      "Check that the dates are not in the future and that you are online.") }

  missing_count = nrow(requests) - length(hourly_rasters)
  if (missing_count > 0) {
    warning(
      missing_count, " of ", nrow(requests),
      " requested hours were not available in the HRRR archive and were dropped.") }

  ## GRIB files store smoke in kilograms (per cubic meter for "surface", per
  ## square meter for "column"); convert to the micrograms / milligrams
  ## documented above, which match how air quality figures are usually reported
  unit_factor = switch(variable, surface = 1e9, column = 1e6)
  smoke_stack = terra::rast(hourly_rasters) * unit_factor

  ## crop to the area of interest in the model's native projection. The bounding
  ## box is buffered by one cell (3 km) so the area's edges are fully covered.
  area_of_interest = geometries %>%
    sf::st_transform(sf::st_crs(smoke_stack)) %>%
    sf::st_bbox() %>%
    sf::st_as_sfc() %>%
    sf::st_buffer(3000) %>%
    terra::vect()

  terra::crop(smoke_stack, area_of_interest)
}
