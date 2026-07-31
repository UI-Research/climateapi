#' @importFrom magrittr %>%

#' @title Acquire FEMA floodplain boundaries
#'
#' @description Retrieves 100-year and 500-year floodplain polygons from FEMA's
#'   National Flood Hazard Layer (NFHL) via its ArcGIS REST API. When FEMA's
#'   service is unavailable, falls back to an Esri-hosted copy of the same layer.
#'
#' @param bbox An sf::st_bbox() object, or an object that can be converted to such
#'   (for example, an sf dataframe). Required: the NFHL is a national dataset far
#'   too large to download in full, so results are limited to flood zones that
#'   intersect this bounding box. Coordinates are assumed to be in the coordinate
#'   reference system of the supplied object (EPSG:4326 if the object has no CRS).
#' @param floodplains Which floodplain categories to return. One or both of
#'   "100-year" and "500-year". Defaults to both.
#' @param silent Logical. When FALSE (the default), a message describing the
#'   returned data is printed, as is a message when the Esri fallback is used and
#'   a warning when no polygons match. When TRUE, all messages and warnings are
#'   suppressed; only an error (if the data cannot be obtained) is raised.
#'
#' @details Data are from the National Flood Hazard Layer's "Flood Hazard Zones"
#'   layer (layer 28). See
#'   \url{https://hazards.fema.gov/arcgis/rest/services/public/NFHL/MapServer/28}.
#'
#'   The 100-year floodplain (more precisely, areas with a one percent or greater
#'   annual chance of flooding, also called the Special Flood Hazard Area)
#'   comprises zones A, AE, AH, AO, AR, A99, V, VE, and VO. The 500-year
#'   floodplain (areas with between a 0.2 percent and one percent annual chance of
#'   flooding) comprises the portions of zone X whose zone subtype begins with
#'   "0.2", which covers all of the wordings FEMA uses for the 0.2 percent annual
#'   chance flood hazard, including the variants for flooding contained in a
#'   channel or structure and for coastal zones. Areas of minimal flood hazard
#'   (the remainder of zone X), open water, and unmapped areas (zone D) are not
#'   returned.
#'
#'   Because large requests can overwhelm the NFHL service, the query proceeds in
#'   two steps: it first retrieves the identifiers of all matching polygons, then
#'   downloads their geometries in batches, retrying each request up to three
#'   times.
#'
#'   FEMA's service is the primary source. When it cannot be reached, the function
#'   falls back to an Esri-hosted copy of the same FEMA layer, at
#'   \url{https://services5.arcgis.com/7weheFjxuNkGGiZi/arcgis/rest/services/USA_Flood_Hazard_Areas_view/FeatureServer/0}.
#'   Esri updates that copy annually, so it can be less current than FEMA's own
#'   service. A message reports whenever the fallback is used. The two sources
#'   write their zone subtype values differently (FEMA uses upper case and
#'   abbreviates "percent" as "PCT"; Esri uses title case and spells out
#'   "Percent"), so values from both are converted to a single form: upper case,
#'   with "PCT" spelled out as "PERCENT".
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
#'       "0.2 PERCENT ANNUAL CHANCE FLOOD HAZARD").}
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

  ## FEMA's own NFHL service is the authoritative source; the Esri-hosted copy of
  ## the same layer is used only when FEMA's service cannot be reached
  url_fema = "https://hazards.fema.gov/arcgis/rest/services/public/NFHL/MapServer/28/query"
  url_esri = paste0(
    "https://services5.arcgis.com/7weheFjxuNkGGiZi/arcgis/rest/services/",
    "USA_Flood_Hazard_Areas_view/FeatureServer/0/query")

  ## the Special Flood Hazard Area (SFHA_TF = "T") is the 100-year floodplain; the
  ## 500-year floodplain is the subset of non-SFHA zone X whose ZONE_SUBTY begins
  ## with "0.2", which captures every wording FEMA and Esri use for the 0.2 percent
  ## annual chance flood hazard
  where_clauses = c(
    if ("100-year" %in% floodplains) "SFHA_TF = 'T'",
    if ("500-year" %in% floodplains) "ZONE_SUBTY LIKE '0.2%'")
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

  ## both services reject or time out on large single requests, so each query
  ## proceeds in two steps: (1) fetch the identifiers of all matching polygons,
  ## then (2) download their geometries in batches, retrying each request as
  ## needed. This returns an sf dataframe (possibly with zero rows, when the bbox
  ## genuinely contains no matching polygons) and raises an error when the service
  ## cannot be reached, which is what triggers the fallback.
  fetch_flood_hazard_zones = function(query_url, source_label) {

    query_with_retries = function(request_body_parameters, request_label) {
      max_attempts = 3
      for (attempt in seq_len(max_attempts)) {
        response = tryCatch(
          expr = {
            httr2::request(query_url) %>%
              httr2::req_body_form(!!!request_body_parameters) %>%
              httr2::req_perform() %>%
              httr2::resp_body_string() },
          error = function(e) { NULL })
        ## the services sometimes return HTTP 200 with an error payload; treat
        ## that as a failed attempt too
        if (!is.null(response) && !stringr::str_detect(response, "^\\s*\\{\"error\"")) {
          return(response) }
        if (attempt < max_attempts) { Sys.sleep(3 * attempt) }
      }
      stop(stringr::str_c(
        "The ", source_label, " service failed to respond to the ", request_label,
        " after ", max_attempts, " attempts.")) }

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

    if (length(object_ids) == 0) {
      return(
        tibble::tibble(
            fld_zone = character(0),
            zone_subty = character(0),
            sfha_tf = character(0),
            static_bfe = numeric(0),
            dfirm_id = character(0)) %>%
          sf::st_sf(geometry = sf::st_sfc(crs = 4326))) }

    batch_size = 100
    object_id_batches = split(object_ids, ceiling(seq_along(object_ids) / batch_size))

    df_zones = purrr::imap(
        object_id_batches,
        function(batch_ids, batch_index) {
          batch_response = query_with_retries(
            list(
              objectIds = stringr::str_c(batch_ids, collapse = ","),
              outFields = "FLD_ZONE,ZONE_SUBTY,SFHA_TF,STATIC_BFE,DFIRM_ID",
              outSR = "4326",
              f = "json"),
            request_label = stringr::str_c(
              "polygon batch ", batch_index, " of ", length(object_id_batches)))
          response_file = tempfile(fileext = ".json")
          on.exit(unlink(response_file), add = TRUE)
          readr::write_file(batch_response, response_file)
          sf::st_read(response_file, quiet = TRUE) }) %>%
      purrr::list_rbind() %>%
      sf::st_as_sf() %>%
      janitor::clean_names()

    if (nrow(df_zones) != length(object_ids)) {
      stop(stringr::str_c(
        "The ", source_label, " service reports ", length(object_ids),
        " matching flood zone polygon(s), but ", nrow(df_zones),
        " were returned.")) }

    return(df_zones) }

  df_floodplains1 = tryCatch(
    expr = { fetch_flood_hazard_zones(url_fema, source_label = "FEMA NFHL") },
    error = function(fema_error) {
      if (!silent) {
        message(stringr::str_c(
          "FEMA's National Flood Hazard Layer service could not be reached (",
          conditionMessage(fema_error), ") Falling back to the Esri-hosted copy ",
          "of the same FEMA layer, which Esri updates annually and which may ",
          "therefore be less current than FEMA's own service.")) }
      tryCatch(
        expr = { fetch_flood_hazard_zones(url_esri, source_label = "Esri fallback") },
        error = function(esri_error) {
          stop(stringr::str_c(
            "Floodplain polygons could not be obtained. FEMA's service reported: ",
            conditionMessage(fema_error), " The Esri fallback reported: ",
            conditionMessage(esri_error),
            " Both services may be temporarily unavailable; please retry."))})})

  if (nrow(df_floodplains1) == 0) {
    if (!silent) {
      warning(stringr::str_c(
        "No floodplain polygons intersect the supplied `bbox`. This can occur ",
        "because the area genuinely contains no mapped flood zones, or because ",
        "the area lacks effective digital flood insurance rate maps in the NFHL.")) }
    return(
      tibble::tibble(
          floodplain = character(0),
          flood_zone = character(0),
          flood_zone_subtype = character(0),
          is_special_flood_hazard_area = logical(0),
          static_base_flood_elevation_feet = numeric(0),
          flood_insurance_rate_map_panel_id = character(0)) %>%
        sf::st_sf(geometry = sf::st_sfc(crs = 4326))) }

  df_floodplains2 = df_floodplains1 %>%
    ## FEMA writes these values in upper case and abbreviates "percent" as "PCT";
    ## Esri writes the same values in title case and spells "Percent" out. Both are
    ## converted to upper case with "PERCENT" spelled out so that the returned
    ## values do not depend on which service answered.
    dplyr::mutate(
      dplyr::across(
        c("fld_zone", "zone_subty", "sfha_tf", "dfirm_id"),
        function(x) {
          x %>%
            as.character() %>%
            stringr::str_to_upper() %>%
            stringr::str_replace_all("(?<=[0-9])PCT", " PCT") %>%
            stringr::str_replace_all("PCT(?![A-Z])", "PERCENT") %>%
            stringr::str_replace_all("NONACCREDITED", "NON-ACCREDITED") %>%
            stringr::str_squish() %>%
            dplyr::na_if("") %>%
            dplyr::na_if("<NULL>") %>%
            dplyr::na_if("NULL") })) %>%
    dplyr::transmute(
      floodplain = dplyr::if_else(sfha_tf == "T", "100-year", "500-year"),
      flood_zone = fld_zone,
      flood_zone_subtype = zone_subty,
      is_special_flood_hazard_area = sfha_tf == "T",
      ## the services use -9999 (FEMA) and -8888 (Esri) as sentinels for "no static
      ## base flood elevation"; genuine below-sea-level elevations are far smaller
      ## in magnitude
      static_base_flood_elevation_feet = dplyr::if_else(
        !is.na(static_bfe) & static_bfe <= -8888, NA_real_, as.numeric(static_bfe)),
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
