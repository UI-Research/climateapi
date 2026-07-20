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
#' @returns An sf dataframe comprising wildfire burn zone disasters, with one row per
#'   wildfire x affected county (a wildfire spanning multiple counties appears as multiple
#'   rows). `geometry`, `area_sq_km`, and the other wildfire-level summary columns
#'   (`fatalities_total`, `injuries_total`, `structures_destroyed`, `structures_threatened`,
#'   `evacuation_total`, `wui_type`, `density_people_sq_km_wildfire_buffer`) are wildfire-level
#'   and repeat identically across a multi-county wildfire's rows; de-duplicate on
#'   `wildfire_id` before summing these columns or unioning geometries (e.g.
#'   `sum(area_sq_km[!duplicated(wildfire_id)])`) to avoid over-counting.
#'   Columns include:
#'   \describe{
#'     \item{wildfire_id}{Unique identifier for the wildfire event.}
#'     \item{id_fema}{FEMA disaster identifier (if applicable).}
#'     \item{year}{Year of the wildfire.}
#'     \item{wildfire_name}{Name of the wildfire or fire complex.}
#'     \item{state_fips}{Two-digit state FIPS code, derived from `county_fips`.}
#'     \item{county_fips}{Five-digit county FIPS code for a single county affected by the wildfire.}
#'     \item{county_name}{Name of a single county affected by the wildfire, sourced from
#'       Census's own canonical county names (joined on `county_fips`) rather than the raw
#'       source data, to avoid mangling Mc-prefixed county names.}
#'     \item{area_sq_km}{Burned area in square kilometers (wildfire-level; see above).}
#'     \item{wildfire_complex_binary}{Whether the fire is a complex (multiple fires).}
#'     \item{date_start}{Ignition date.}
#'     \item{date_containment}{Containment date.}
#'     \item{fatalities_total}{Total fatalities (wildfire-level; see above).}
#'     \item{injuries_total}{Total injuries (wildfire-level; see above).}
#'     \item{structures_destroyed}{Number of structures destroyed (wildfire-level; see above).}
#'     \item{structures_threatened}{Number of structures threatened (wildfire-level; see above).}
#'     \item{evacuation_total}{Total evacuations (wildfire-level; see above).}
#'     \item{wui_type}{Wildland-urban interface type (wildfire-level; see above).}
#'     \item{density_people_sq_km_wildfire_buffer}{Population density in wildfire buffer
#'       area (wildfire-level; see above).}
#'     \item{geometry}{Burn zone polygon geometry (wildfire-level; see above).}
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
      density_people_sq_km_wildfire_buffer = wildfire_buffered_avg_pop_den) |>
    ## one row per wildfire x affected county (previously one row per wildfire, with all
    ## affected counties packed into pipe-delimited strings); county_fips and county_name
    ## always have matching pipe-delimited counts per record, so exploding both in lockstep
    ## keeps them correctly paired
    ## tidyr::separate_longer_delim() drops the sf class (though the geometry list-column
    ## itself stays intact as sfc), so it must be re-wrapped with sf::st_as_sf()
    tidyr::separate_longer_delim(c(county_fips, county_name), delim = "|") |>
    sf::st_as_sf() |>
    dplyr::mutate(
      county_fips = stringr::str_trim(county_fips),
      state_fips = stringr::str_sub(county_fips, 1, 2)) |>
    ## county_name is re-derived from Census's own canonical county names (keyed on
    ## county_fips) rather than title-casing the raw source string, which would mangle
    ## Mc-prefixed county names (e.g. str_to_title("MCKENZIE") -> "Mckenzie", not "McKenzie")
    dplyr::select(-county_name) |>
    dplyr::left_join(
      tidycensus::fips_codes |>
        dplyr::transmute(county_fips = stringr::str_c(state_code, county_code), county_name = county),
      by = "county_fips",
      relationship = "many-to-one") |>
    dplyr::relocate(state_fips, county_fips, county_name, .after = wildfire_name)

  message(stringr::str_c(
    "Each observation represents a county affected by a wildfire burn zone disaster. ",
    "Wildfires spanning multiple counties appear as multiple rows. ",
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
  "wildfire_wui", "wildfire_buffered_avg_pop_den", "state_code", "county_code", "county",
  "state_fips", "county_fips", "county_name", "wildfire_name"))
