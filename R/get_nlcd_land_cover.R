#' Get vectorized Annual NLCD land cover data
#'
#' @description
#' Summarizes Annual National Land Cover Database (NLCD) land cover -- a 30-meter
#' categorical raster -- to Census tracts (by default) or to a set of user-provided
#' geometries, returning, for each land cover class, both its share of the geometry's
#' area and its area in square meters.
#'
#' @details
#' Land cover is drawn from the Annual NLCD product (Multi-Resolution Land
#' Characteristics Consortium). As of the Collection 1.1 release, Annual NLCD provides a
#' continuous annual time series for the conterminous United States spanning 1985 through
#' 2024. Because land cover is categorical, each geometry is summarized as the
#' coverage-weighted fraction of its area falling in each of the NLCD land cover classes
#' (via `exactextractr::exact_extract(fun = "frac")`) rather than as a single average
#' value.
#'
#' The raster download is delegated to `FedData::get_nlcd_annual()`. Because the Annual
#' NLCD product is currently published only for the conterminous US, Alaska, Hawaii, and
#' the territories are unsupported.
#'
#' @param county_geoids A vector of five-character county GEOID(s). Used to fetch tract
#'   geometries when `geometries` is not supplied; ignored when `geometries` is supplied.
#' @param year One or more years for which to retrieve land cover, each between 1985 and
#'   2024 (the range covered by the Annual NLCD product). Defaults to 2024, the most
#'   recent vintage.
#' @param geometries An `sf`-formatted dataframe specifying the geometries to which NLCD
#'   land cover should be summarized. Must contain a `GEOID` column and have a defined
#'   coordinate reference system. Default is `NULL`, in which case land cover is
#'   summarized to Census tracts for the counties named in `county_geoids`.
#' @param return_raw Logical. If `TRUE`, the raw (cropped) NLCD raster(s) returned by
#'   `FedData::get_nlcd_annual()` are attached to the result as an attribute named
#'   `"nlcd_raster"`.
#'
#' @return A tibble with one row per geometry, year, and land cover class present in that
#'   geometry:
#' \describe{
#'   \item{GEOID}{Character. The geometry identifier (tract GEOID, or the `GEOID` of the
#'     supplied `geometries`).}
#'   \item{year}{Integer. The NLCD vintage.}
#'   \item{land_cover_code}{Integer. The NLCD land cover class code (e.g. 11, 21, 42).}
#'   \item{land_cover_class}{Character. The NLCD class name (e.g. "Open Water",
#'     "Deciduous Forest").}
#'   \item{area_fraction}{Numeric. The coverage-weighted share (0-1) of the geometry's
#'     own area falling in this land cover class. A within-geometry proportion -- not
#'     comparable across geometries on its own; use `area_sq_meters` for that.}
#'   \item{area_sq_meters}{Numeric. The absolute area, in square meters, of this record --
#'     i.e. of this land cover class within this geometry and year. Comparable across
#'     geometries. Sum it by `GEOID` (optionally with `year`) to recover a geometry's
#'     total area.}
#' }
#' When `return_raw = TRUE`, the raw NLCD rasters (the tibble returned by
#' `FedData::get_nlcd_annual()`, carrying a `rast` list-column of `SpatRaster`s) are
#' attached to the result as the `"nlcd_raster"` attribute.
#' @export
#'
#' @examples
#' \dontrun{
#' # Land cover for every tract in the District of Columbia, 2024
#' get_nlcd_land_cover(county_geoids = "11001")
#'
#' # A multi-year panel over a user-supplied set of geometries
#' geographies = tigris::tracts(state = "24", county = "031", cb = TRUE) %>%
#'   dplyr::select(GEOID)
#' get_nlcd_land_cover(geometries = geographies, year = c(2001, 2012, 2024))
#' }
get_nlcd_land_cover = function(
    county_geoids = NULL,
    year = 2024,
    geometries = NULL,
    return_raw = FALSE) {

  year = as.integer(year)

  ## the Annual NLCD product currently spans 1985-2024 (Collection 1.1) for the
  ## conterminous US; bump the upper bound when MRLC publishes a new vintage
  if (any(is.na(year)) || any(year < 1985) || any(year > 2024)) {
    stop("`year` must contain only integer years between 1985 and 2024 (the range covered by the Annual NLCD product).") }

  ## work in CONUS Albers Equal Area (EPSG:5070); its units are meters, so areas come out
  ## in square meters. Annual NLCD is CONUS-only, so this equal-area CRS always applies.
  working_crs = 5070

  ## resolve the geometries over which land cover will be summarized -------------------
  if (is.null(geometries)) {
    if (is.null(county_geoids)) {
      stop("You must supply either `county_geoids` or `geometries`.") }
    if (!all(nchar(county_geoids) == 5)) {
      stop("`county_geoids` must be a vector of five-character (county-GEOID) element(s).") }

    ## fetch tracts county-by-county so that counties spanning multiple states are
    ## handled correctly (tigris::tracts() expects a single state per call)
    geometries = suppressMessages(purrr::map_dfr(
        county_geoids,
        ~ tigris::tracts(
            state = stringr::str_sub(.x, 1, 2),
            county = stringr::str_sub(.x, 3, 5),
            year = 2023,
            cb = TRUE,
            progress_bar = FALSE))) %>%
      dplyr::select(GEOID) %>%
      sf::st_transform(working_crs)
  } else {
    if (!inherits(geometries, "sf")) {
      stop("`geometries` must be a simple features (sf) object.") }
    if (is.na(sf::st_crs(geometries))) {
      stop("`geometries` must have a defined coordinate reference system (CRS).") }
    if (!("GEOID" %in% colnames(geometries))) {
      stop("`geometries` must contain a column named 'GEOID'.") }

    geometries = sf::st_transform(geometries, working_crs)
  }

  ## total area of each geometry, in square meters (geometries are in the equal-area,
  ## meter-based EPSG:5070). Used internally to convert the within-geometry `area_fraction`
  ## into an absolute per-record area; the total itself is not returned (sum
  ## `area_sq_meters` by GEOID to recover it).
  geometry_areas = tibble::tibble(
      GEOID = geometries$GEOID,
      feature_area = as.numeric(sf::st_area(geometries))) %>%
    dplyr::summarize(geometry_area_total = sum(feature_area), .by = GEOID)

  ## download + crop the annual land cover raster(s). FedData returns a tibble with one
  ## row per requested year and a `rast` list-column of SpatRasters. `force.redo = TRUE`
  ## is required for correctness: FedData caches extractions by label (not by template),
  ## so without it a second call would silently reuse the first call's cropped raster.
  nlcd_raw = suppressWarnings(suppressMessages(
    FedData::get_nlcd_annual(
      template = geometries,
      label = "climateapi_nlcd",
      year = year,
      product = "LndCov",
      force.redo = TRUE)))

  ## NLCD class-code -> class-name lookup (e.g. 42 -> "Evergreen Forest")
  class_lookup = get_nlcd_land_cover_colors() %>%
    dplyr::select(land_cover_code, land_cover_class)

  ## for each year's raster, compute the coverage-weighted share of every geometry's area
  ## in each land cover class. `fun = "frac"` is the categorical-raster analogue of a mean
  ## (columns come back as `frac_<code>`); exact_extract reprojects `geometries` to the
  ## raster's CRS internally.
  df1 = purrr::map2_dfr(
    nlcd_raw$year,
    nlcd_raw$rast,
    function(nlcd_year, nlcd_raster) {
      exactextractr::exact_extract(
          x = nlcd_raster,
          y = geometries,
          fun = "frac",
          append_cols = "GEOID",
          progress = FALSE) %>%
        tibble::as_tibble() %>%
        tidyr::pivot_longer(
          cols = dplyr::starts_with("frac_"),
          names_to = "land_cover_code",
          names_prefix = "frac_",
          values_to = "area_fraction") %>%
        dplyr::mutate(
          year = nlcd_year,
          land_cover_code = as.integer(land_cover_code)) })

  df2 = df1 %>%
    dplyr::left_join(class_lookup, by = "land_cover_code", relationship = "many-to-one") %>%
    dplyr::filter(area_fraction > 0) %>%
    dplyr::left_join(geometry_areas, by = "GEOID", relationship = "many-to-one") %>%
    dplyr::mutate(area_sq_meters = area_fraction * geometry_area_total) %>%
    dplyr::select(
      GEOID, year, land_cover_code, land_cover_class, area_fraction, area_sq_meters) %>%
    dplyr::arrange(GEOID, year, dplyr::desc(area_fraction))

  if (isTRUE(return_raw)) {
    attr(df2, "nlcd_raster") = nlcd_raw }

  return(df2)
}

#' Get NLCD land cover classes and their standard colors
#'
#' @description
#' Returns the Annual National Land Cover Database (NLCD) land cover
#' classification as a lookup table: each land cover class, its numeric code, the
#' standard NLCD display color (as a hex code), and a short description. This is
#' the same class-and-color reference that `get_nlcd_land_cover()` uses
#' internally, exposed so that you can label and color maps or charts by land
#' cover class without depending on the FedData package directly.
#'
#' @return A tibble with one row per NLCD land cover class:
#' \describe{
#'   \item{land_cover_code}{Integer. The NLCD land cover class code (e.g. 11, 21,
#'     42).}
#'   \item{land_cover_class}{Character. The NLCD class name (e.g. "Open Water",
#'     "Deciduous Forest").}
#'   \item{color}{Character. The standard NLCD display color for the class, as a
#'     hex code (e.g. "#5475A8").}
#'   \item{description}{Character. A short description of the land cover class.}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' # the class-to-color lookup used to color NLCD land cover maps
#' get_nlcd_land_cover_colors()
#' }
get_nlcd_land_cover_colors = function() {
  FedData::nlcd_colors() %>%
    dplyr::transmute(
      land_cover_code = as.integer(ID),
      land_cover_class = Class,
      color = Color,
      description = Description)
}

utils::globalVariables(c(
  "GEOID", "ID", "Class", "Color", "Description", "area_fraction",
  "land_cover_code", "land_cover_class", "feature_area", "geometry_area_total",
  "area_sq_meters"))
