#' @title Get wildfire burn zones
#'
#' @description Returns spatial data on wildfire burn zones in the US from 2000-2025.
#'   This dataset harmonizes six wildfire datasets (FIRED, MTBS, NIFC, ICS-209, RedBook, and FEMA)
#'   to identify wildfires that burned near communities and resulted in civilian fatalities,
#'   destroyed structures, or received federal disaster relief.
#'
#' @param file_path The path to the geojson file containing the raw data. Defaults to a
#'   path within Box.
#'
#' @details Data are from a harmonized wildfire burn zone disaster dataset combining
#'   FIRED, MTBS, NIFC, ICS-209, RedBook, and FEMA data sources. Geometries are in
#'   NAD83 / Conus Albers (EPSG:5070).
#'
#' @returns An sf dataframe comprising wildfire burn zone disasters. Each row represents a
#'   single wildfire event, with polygon geometries representing burn zones.
#'   Columns include:
#'   \describe{
#'     \item{wildfire_id}{Unique identifier for the wildfire event.}
#'     \item{id_fema}{FEMA disaster identifier (if applicable).}
#'     \item{year}{Year of the wildfire.}
#'     \item{wildfire_name}{Name of the wildfire or fire complex.}
#'     \item{county_fips}{Pipe-delimited string of five-digit county FIPS codes for all
#'       counties affected by the wildfire.}
#'     \item{county_name}{Pipe-delimited string of county names for all counties
#'       affected by the wildfire.}
#'     \item{area_sq_km}{Burned area in square kilometers.}
#'     \item{wildfire_complex_binary}{Whether the fire is a complex (multiple fires).}
#'     \item{date_start}{Ignition date.}
#'     \item{date_containment}{Containment date.}
#'     \item{fatalities_total}{Total fatalities.}
#'     \item{injuries_total}{Total injuries.}
#'     \item{structures_destroyed}{Number of structures destroyed.}
#'     \item{structures_threatened}{Number of structures threatened.}
#'     \item{evacuation_total}{Total evacuations.}
#'     \item{wui_type}{Wildland-urban interface type.}
#'     \item{density_people_sq_km_wildfire_buffer}{Population density in wildfire buffer area.}
#'     \item{geometry}{Burn zone polygon geometry.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' burn_zones <- get_wildfire_burn_zones()
#' }
get_wildfire_burn_zones <- function(
    file_path = file.path(
      get_box_path(), "hazards", "other-sources", "wildfire-burn-zones",
      "wfbz_disasters_2000-2025.geojson")) {

  if (!file.exists(file_path)) {
    stop(stringr::str_c(
      "The path to the dataset does not point to a valid file. ",
      "Please ensure there is a file located at this path: ", file_path, "."))
  }

  burn_zones1 <- sf::st_read(file_path, quiet = TRUE) |>
    janitor::clean_names() |>
    sf::st_transform(5070)

  burn_zones2 <- burn_zones1 |>
    dplyr::transmute(
      wildfire_id = wildfire_id,
      id_fema = fema_id,
      year = as.integer(wildfire_year),
      wildfire_name = wildfire_complex_names,
      county_fips = wildfire_counties_fips,
      county_name = wildfire_counties,
      area_sq_km = wildfire_area,
      wildfire_complex_binary = wildfire_complex,
      date_start = lubridate::as_date(wildfire_ignition_date),
      date_containment = lubridate::as_date(wildfire_containment_date),
      fatalities_total = as.integer(wildfire_total_fatalities),
      injuries_total = as.integer(wildfire_total_injuries),
      structures_destroyed = as.integer(wildfire_struct_destroyed),
      structures_threatened = as.integer(wildfire_struct_threatened),
      evacuation_total = as.integer(wildfire_total_evacuation),
      wui_type = wildfire_wui,
      ## codebook says this is per square meter, but that must be a typo based on distribution of density values
      ## other values are per square kilometer, which makes more sense
      density_people_sq_km_wildfire_buffer = wildfire_buffered_avg_pop_den)

  message(stringr::str_c(
    "Each observation represents a wildfire burn zone disaster. ",
    "Counties affected by each wildfire are stored as pipe-delimited strings in county_fips and county_name columns. ",
    "Disasters are defined as wildfires that burned near a community and resulted in ",
    "at least one civilian fatality, one destroyed structure, or received federal disaster relief. ",
    "Geometries represent burn zone perimeters sourced from FIRED, MTBS, or NIFC datasets."))

  return(burn_zones2)
}

utils::globalVariables(c(
  "wildfire_id", "fema_id", "wildfire_year", "wildfire_complex_names", "wildfire_counties",
  "wildfire_counties_fips", "wildfire_area", "wildfire_complex", "wildfire_ignition_date",
  "wildfire_containment_date", "wildfire_total_fatalities", "wildfire_total_injuries",
  "wildfire_struct_destroyed", "wildfire_struct_threatened", "wildfire_total_evacuation",
  "wildfire_wui", "wildfire_buffered_avg_pop_den"))
