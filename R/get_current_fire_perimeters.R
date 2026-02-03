#' @importFrom magrittr %>%

#' @title Acquire current wildfire perimeters
#'
#' @description Retrieves current wildfire perimeter data from the NIFC (National
#'   Interagency Fire Center) via the Wildland Fire Interagency Geospatial Services
#'   (WFIGS) API.
#'
#' @param geography Included only for API consistency; this must be NULL.
#' @param file_path Included only for API consistency; this must be NULL.
#' @param bbox Optionally, an sf::st_bbox() object, or an object that can be converted to such.
#' @param api Included only for API consistency; this must be TRUE.
#'
#' @details Data are from the NIFC WFIGS service. See
#'   \url{https://data-nifc.opendata.arcgis.com/datasets/nifc::wfigs-interagency-fire-perimeters/about}.
#'
#' @returns An sf dataframe comprising perimeters of current wildfires. Columns include:
#'   \describe{
#'     \item{unique_id}{Unique identifier for each observation (generated).}
#'     \item{incident_name}{Name of the fire incident (title case).}
#'     \item{incident_size_acres}{Size of the fire in acres.}
#'     \item{incident_short_description}{Brief description of the incident.}
#'     \item{percent_contained}{Percent of fire contained (0-100).}
#'     \item{identified_date}{Date/time the fire was discovered.}
#'     \item{updated_date}{Date/time the record was last updated.}
#'     \item{geometry}{Polygon geometry of the fire perimeter.}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' get_current_fire_perimeters()
#' }
get_current_fire_perimeters = function(
  geography = NULL,
  file_path = NULL,
  bbox = NULL,
  api = TRUE) {

  ## if specified, is `bbox` a valid sf::st_bbox() object?
  if (!is.null(bbox)) {
    bbox = tryCatch(
      expr = { sf::st_bbox(bbox) },
      error = function(e) {
        warning(e)
        stop("Please specify a valid bbox object using sf::st_bbox(), or at minimum,
             an object that can be converted to such.")})}

  ## is a valid geography option provided? (must be NULL)
  if (!is.null(geography)) {
    stop("Invalid geography. Geography must be NULL.")}

  ## data may only be queried from the API
  if (!isTRUE(api)) {
    stop("Data must be queried from the API; set `api = TRUE`.")}

  url_perimeters = "https://services3.arcgis.com/T4QMspbfLg3qTGWY/arcgis/rest/services/WFIGS_Interagency_Perimeters_Current/FeatureServer/0"

  df_perimeters1 = esri2sf::esri2sf(url = url_perimeters, bbox = bbox) %>%
    janitor::clean_names() %>%
    dplyr::transmute(
      unique_id = uuid::UUIDgenerate(n = nrow(.)),
      incident_name = attr_incident_name %>% stringr::str_to_title(),
      incident_size_acres = attr_incident_size,
      incident_short_description = attr_incident_short_description,
      percent_contained = attr_percent_contained,
      identified_date = attr_fire_discovery_date_time %>% lubridate::as_datetime(),
      updated_date = attr_modified_on_date_time_dt %>% lubridate::as_datetime())

  message(stringr::str_c(
    "Each observation represents a distinct wildfire and its perimeters. ",
    "Observations returned are those that intersect with the argument supplied ",
    "to `bbox`. If no argument was supplied, observations returned reflect all current ",
    "wildfires in the US. ",
    "The `unique_id` field is a unique identifier for each observation. "))

  return(df_perimeters1)
}

utils::globalVariables(c(
  "attr_incident_name", "attr_incident_size", "attr_incident_short_description",
  "attr_percent_contained", "attr_fire_discovery_date_time", ".",
  "attr_modified_on_date_time_dt"))
