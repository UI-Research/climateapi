#' @importFrom magrittr %>%

#' @title Acquire FEMA floodplain boundaries
#'
#' @description Retrieves 100-year and 500-year floodplain polygons from FEMA's
#'   National Flood Hazard Layer (NFHL) via its ArcGIS REST API.
#'
#' @param bbox An sf::st_bbox() object, or an object that can be converted to such
#'   (for example, an sf dataframe). Required: the NFHL is a national dataset far
#'   too large to download in full, so results are limited to flood zones that
#'   intersect this bounding box. Coordinates are assumed to be in the coordinate
#'   reference system of the supplied object (EPSG:4326 if the object has no CRS).
#' @param floodplains Which floodplain categories to return. One or both of
#'   "100-year" and "500-year". Defaults to both.
#' @param silent Logical. When FALSE (the default), a message describing the
#'   returned data is printed, as is a warning when no polygons match. When TRUE,
#'   all messages and warnings are suppressed; only an error (if the data cannot
#'   be obtained) is raised.
#'
#' @details Data are from the National Flood Hazard Layer's "Flood Hazard Zones"
#'   layer (layer 28). See
#'   \url{https://hazards.fema.gov/arcgis/rest/services/public/NFHL/MapServer/28}.
#'
#'   The 100-year floodplain (more precisely, areas with a one percent or greater
#'   annual chance of flooding, also called the Special Flood Hazard Area)
#'   comprises zones A, AE, AH, AO, AR, A99, V, VE, and VO. The 500-year
#'   floodplain (areas with between a 0.2 percent and one percent annual chance of
#'   flooding) comprises the portions of zone X flagged as "0.2 PCT ANNUAL CHANCE
#'   FLOOD HAZARD". Areas of minimal flood hazard (the remainder of zone X), open
#'   water, and unmapped areas (zone D) are not returned.
#'
#'   Because large requests can overwhelm the NFHL service, the function first
#'   retrieves the identifiers of all matching polygons and then downloads their
#'   geometries in batches, retrying each request up to three times.
#'
#'   Note that the NFHL only covers communities with effective digital flood
#'   insurance rate maps; areas without digital maps return no polygons even
#'   though they may face flood risk.
#'
#' @returns An sf dataframe comprising floodplain polygons. Columns include:
#'   \describe{
#'     \item{floodplain}{Floodplain category: "100-year" or "500-year".}
#'     \item{flood_zone}{The FEMA flood zone designation (e.g., "AE", "VE", "X").}
#'     \item{flood_zone_subtype}{Additional zone detail (e.g., "FLOODWAY",
#'       "0.2 PCT ANNUAL CHANCE FLOOD HAZARD").}
#'     \item{is_special_flood_hazard_area}{Logical. TRUE when the polygon is part
#'       of the Special Flood Hazard Area (the 100-year floodplain).}
#'     \item{static_base_flood_elevation_feet}{The static base flood elevation, in
#'       feet, where one applies (NA otherwise).}
#'     \item{flood_insurance_rate_map_panel_id}{The identifier of the source flood
#'       insurance rate map panel.}
#'     \item{geometry}{Polygon geometry of the flood zone, in EPSG:4326.}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' bbox = sf::st_bbox(
#'   c(xmin = -77.05, ymin = 38.87, xmax = -77.00, ymax = 38.91),
#'   crs = 4326)
#' get_fema_floodplain(bbox = bbox)
#' get_fema_floodplain(bbox = bbox, floodplains = "100-year")
#' }
get_fema_floodplain = function(
  bbox,
  floodplains = c("100-year", "500-year"),
  silent = FALSE) {

  if (missing(bbox) || is.null(bbox)) {
    stop("`bbox` is required: the NFHL is a national dataset and must be subset
         geographically. Supply an sf::st_bbox() object or an object that can be
         converted to such.") }

  bbox = tryCatch(
    expr = { sf::st_bbox(bbox) },
    error = function(e) {
      warning(e)
      stop("Please specify a valid bbox object using sf::st_bbox(), or at minimum,
           an object that can be converted to such.")})

  if (!all(floodplains %in% c("100-year", "500-year")) || length(floodplains) == 0) {
    stop("`floodplains` must be one or both of '100-year' and '500-year'.") }

  ## NFHL layer 28 is the "Flood Hazard Zones" (S_Fld_Haz_Ar) layer
  url_flood_hazard_zones = "https://hazards.fema.gov/arcgis/rest/services/public/NFHL/MapServer/28/query"

  ## the Special Flood Hazard Area (SFHA_TF = "T") is the 100-year floodplain; the
  ## 500-year floodplain is the subset of non-SFHA zone X so flagged in ZONE_SUBTY
  where_clauses = c(
    if ("100-year" %in% floodplains) "SFHA_TF = 'T'",
    if ("500-year" %in% floodplains) "ZONE_SUBTY = '0.2 PCT ANNUAL CHANCE FLOOD HAZARD'")
  where = stringr::str_c("(", where_clauses, ")", collapse = " OR ")

  ## per the documentation, a bbox without a CRS is assumed to be in EPSG:4326
  if (is.na(sf::st_crs(bbox))) {
    sf::st_crs(bbox) = 4326 }

  bbox_4326 = bbox %>%
    sf::st_as_sfc() %>%
    sf::st_transform(4326) %>%
    sf::st_bbox()

  bbox_json = jsonlite::toJSON(
    list(
      xmin = bbox_4326[["xmin"]], ymin = bbox_4326[["ymin"]],
      xmax = bbox_4326[["xmax"]], ymax = bbox_4326[["ymax"]],
      spatialReference = list(wkid = 4326)),
    auto_unbox = TRUE)

  ## the NFHL service rejects or times out on large single requests, so the query
  ## proceeds in two steps: (1) fetch the identifiers of all matching polygons,
  ## then (2) download their geometries in batches, retrying each request as needed
  query_with_retries = function(request_body_parameters, request_label) {
    max_attempts = 3
    for (attempt in seq_len(max_attempts)) {
      response = tryCatch(
        expr = {
          httr2::request(url_flood_hazard_zones) %>%
            httr2::req_body_form(!!!request_body_parameters) %>%
            httr2::req_perform() %>%
            httr2::resp_body_string() },
        error = function(e) { NULL })
      ## the service sometimes returns HTTP 200 with an error payload; treat that
      ## as a failed attempt too
      if (!is.null(response) && !stringr::str_detect(response, "^\\s*\\{\"error\"")) {
        return(response) }
      if (attempt < max_attempts) { Sys.sleep(3 * attempt) }
    }
    stop(stringr::str_c(
      "The NFHL service failed to respond to the ", request_label, " after ",
      max_attempts, " attempts. The service may be temporarily unavailable; ",
      "please retry.")) }

  ids_response = query_with_retries(
    list(
      where = where,
      geometry = bbox_json,
      geometryType = "esriGeometryEnvelope",
      spatialRel = "esriSpatialRelIntersects",
      returnIdsOnly = "true",
      f = "json"),
    request_label = "polygon-identifier query")

  object_ids = jsonlite::fromJSON(ids_response)$objectIds

  empty_result = tibble::tibble(
      floodplain = character(0),
      flood_zone = character(0),
      flood_zone_subtype = character(0),
      is_special_flood_hazard_area = logical(0),
      static_base_flood_elevation_feet = numeric(0),
      flood_insurance_rate_map_panel_id = character(0)) %>%
    sf::st_sf(geometry = sf::st_sfc(crs = 4326))

  if (length(object_ids) == 0) {
    if (!silent) {
      warning(stringr::str_c(
        "No floodplain polygons intersect the supplied `bbox`. This can occur ",
        "because the area genuinely contains no mapped flood zones, or because ",
        "the area lacks effective digital flood insurance rate maps in the NFHL.")) }
    return(empty_result) }

  batch_size = 100
  object_id_batches = split(object_ids, ceiling(seq_along(object_ids) / batch_size))

  df_floodplains1 = purrr::imap(
      object_id_batches,
      function(batch_ids, batch_index) {
        batch_response = query_with_retries(
          list(
            objectIds = stringr::str_c(batch_ids, collapse = ","),
            outFields = "FLD_ZONE,ZONE_SUBTY,SFHA_TF,STATIC_BFE,DFIRM_ID",
            f = "json"),
          request_label = stringr::str_c(
            "polygon batch ", batch_index, " of ", length(object_id_batches)))
        response_file = tempfile(fileext = ".json")
        on.exit(unlink(response_file), add = TRUE)
        readr::write_file(batch_response, response_file)
        sf::st_read(response_file, quiet = TRUE) }) %>%
    purrr::list_rbind() %>%
    sf::st_as_sf()

  if (nrow(df_floodplains1) != length(object_ids)) {
    stop(stringr::str_c(
      "The NFHL service reports ", length(object_ids), " matching flood zone ",
      "polygon(s), but ", nrow(df_floodplains1), " were returned. The service ",
      "may be temporarily unavailable; please retry.")) }

  df_floodplains2 = df_floodplains1 %>%
    janitor::clean_names() %>%
    dplyr::transmute(
      floodplain = dplyr::if_else(sfha_tf == "T", "100-year", "500-year"),
      flood_zone = fld_zone,
      flood_zone_subtype = zone_subty,
      is_special_flood_hazard_area = sfha_tf == "T",
      ## the service uses -9999 as a sentinel for "no static base flood elevation"
      static_base_flood_elevation_feet = dplyr::if_else(
        static_bfe <= -9999, NA_real_, static_bfe),
      flood_insurance_rate_map_panel_id = dfirm_id) %>%
    sf::st_transform(4326)

  if (!silent) {
    message(stringr::str_c(
      "Each observation represents a flood zone polygon from the National Flood ",
      "Hazard Layer that intersects the supplied `bbox`. Polygons are not clipped ",
      "to the bbox and may extend beyond it. Areas without effective digital ",
      "flood insurance rate maps are absent from the NFHL and return no polygons ",
      "regardless of their actual flood risk.")) }

  return(df_floodplains2)
}

utils::globalVariables(c(
  "sfha_tf", "fld_zone", "zone_subty", "static_bfe", "dfirm_id"))
